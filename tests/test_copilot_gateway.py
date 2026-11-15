import asyncio
import json
import os
import sys
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "backend"))

from bridge import dispatch
from copilot_adapter import CopilotAdapter
from model_gateway import GatewayError, ModelGateway


SCHEMA = {
    "type": "object",
    "properties": {"rule": {"type": "string"}},
    "required": ["rule"],
    "additionalProperties": False,
}


class FakeProvider:
    def __init__(self, text='{"rule": "synthetic"}'):
        self.text = text

    async def models(self):
        return [{"id": "model", "name": "Synthetic model"}]

    async def generate(self, model, prompt, schema):
        return {"text": self.text}


class GatewayTests(unittest.IsolatedAsyncioTestCase):
    async def test_unapproved_provider_and_consent(self):
        gateway = ModelGateway({"copilot": FakeProvider()})
        with self.assertRaises(GatewayError):
            await gateway.models("other")
        with self.assertRaises(GatewayError):
            await dispatch({"action": "models", "provider": "copilot"}, gateway)

    async def test_normalization_and_schema(self):
        gateway = ModelGateway({"copilot": FakeProvider()})
        result = await gateway.generate(
            "copilot", "model", "synthetic_test", "test", SCHEMA
        )
        self.assertEqual(result["structured"]["rule"], "synthetic")
        self.assertEqual(result["provider"], "copilot")
        self.assertEqual(result["model"], "model")
        self.assertIsNone(result["usage"])
        self.assertGreaterEqual(result["latency_ms"], 0)
        self.assertTrue(result["request_id"])

    async def test_invalid_structured_output(self):
        for text in ("not json", json.dumps({"rule": 3})):
            gateway = ModelGateway({"copilot": FakeProvider(text)})
            with self.assertRaises(GatewayError):
                await gateway.generate(
                    "copilot", "model", "synthetic_test", "test", SCHEMA
                )

    async def test_provider_errors_propagate(self):
        class BadProvider(FakeProvider):
            async def generate(self, model, prompt, schema):
                raise GatewayError("connection failed")

        with self.assertRaisesRegex(GatewayError, "connection failed"):
            await ModelGateway({"copilot": BadProvider()}).generate(
                "copilot", "model", "synthetic_test", "test"
            )

    async def test_adapter_denies_tools_and_checks_models(self):
        calls = {}

        class Session:
            async def __aenter__(self):
                return self

            async def __aexit__(self, *args):
                return False

            async def send_and_wait(self, prompt, **kwargs):
                calls["send"] = (prompt, kwargs)
                return SimpleNamespace(data=SimpleNamespace(content="synthetic result"))

        class Client:
            async def __aenter__(self):
                return self

            async def __aexit__(self, *args):
                return False

            async def list_models(self):
                return [SimpleNamespace(id="model", name="Model")]

            async def create_session(self, **kwargs):
                calls["session"] = kwargs
                return Session()

        with patch.object(CopilotAdapter, "_client", return_value=Client()):
            result = await CopilotAdapter().generate("model", "synthetic", None)
            self.assertEqual(result["text"], "synthetic result")
            self.assertEqual(calls["session"]["available_tools"], [])
            self.assertFalse(calls["session"]["enable_skills"])
            self.assertFalse(calls["session"]["enable_session_store"])
            with self.assertRaises(GatewayError):
                await CopilotAdapter().generate("unknown", "synthetic", None)

    async def test_byok_configuration_is_blocked(self):
        with patch.dict(os.environ, {"COPILOT_PROVIDER_API_KEY": "placeholder"}):
            with self.assertRaises(GatewayError):
                async with CopilotAdapter()._client():
                    self.fail("Unapproved provider was accepted")

    async def test_external_runtime_is_loopback_and_cleaned_up(self):
        class FakeProcess:
            returncode = None
            stdout = SimpleNamespace(
                readline=AsyncMock(return_value=b"Listening on port 32123\n")
            )

            def terminate(self):
                self.returncode = 0

            async def wait(self):
                return self.returncode

        class FakeClient:
            async def __aenter__(self):
                return self

            async def __aexit__(self, *args):
                return False

        process = FakeProcess()
        with (
            patch.dict(os.environ, {"ADAM_QC_COPILOT_CLI": __file__}),
            patch(
                "copilot_adapter.asyncio.create_subprocess_exec",
                new_callable=AsyncMock,
                return_value=process,
            ) as spawn,
            patch("copilot.RuntimeConnection.for_uri") as connection,
            patch("copilot.CopilotClient", return_value=FakeClient()) as client,
        ):
            async with CopilotAdapter()._client():
                args, options = spawn.call_args
                self.assertIn("127.0.0.1", args)
                self.assertIn("--disable-builtin-mcps", args)
                token = options["env"]["COPILOT_CONNECTION_TOKEN"]
                self.assertGreaterEqual(len(token), 32)
                self.assertNotIn(token, args)
                connection.assert_called_once_with(
                    "127.0.0.1:32123", connection_token=token
                )
                self.assertEqual(client.call_args.kwargs["mode"], "empty")
            self.assertEqual(process.returncode, 0)


if __name__ == "__main__":
    unittest.main()
