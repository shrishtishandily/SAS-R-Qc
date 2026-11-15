"""Provider-neutral interface for the synthetic-data prototype."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Protocol
from uuid import uuid4
import json
import time


class GatewayError(Exception):
    """A configuration, provider, or response validation failure."""


class Provider(Protocol):
    async def models(self) -> list[dict[str, str]]: ...

    async def generate(self, model: str, prompt: str, schema: dict | None) -> dict: ...


@dataclass
class ModelGateway:
    providers: dict[str, Provider]

    def provider(self, name: str) -> Provider:
        if name != "copilot" or name not in self.providers:
            raise GatewayError(
                "Only the configured GitHub Copilot provider is supported."
            )
        return self.providers[name]

    async def models(self, provider: str) -> list[dict[str, str]]:
        return await self.provider(provider).models()

    async def generate(
        self,
        provider: str,
        model: str,
        task: str,
        prompt: str,
        schema: dict | None = None,
    ) -> dict[str, Any]:
        adapter = self.provider(provider)
        if not isinstance(model, str) or not model.strip():
            raise GatewayError(
                "Select a model returned by the Copilot connection check."
            )
        if (
            task != "synthetic_test"
            or not isinstance(prompt, str)
            or not prompt.strip()
        ):
            raise GatewayError("A nonempty synthetic test prompt is required.")
        if len(prompt) > 10000:
            raise GatewayError("Synthetic prompts are limited to 10,000 characters.")
        start = time.monotonic()
        result = await adapter.generate(model, prompt, schema)
        if not isinstance(result, dict) or not isinstance(result.get("text"), str):
            raise GatewayError("Provider returned an invalid response.")
        structured = None
        if schema is not None:
            try:
                import jsonschema

                structured = json.loads(result["text"])
                jsonschema.validate(structured, schema)
            except (
                ValueError,
                jsonschema.exceptions.ValidationError,
                jsonschema.exceptions.SchemaError,
            ) as error:
                raise GatewayError(
                    "Provider returned invalid structured output."
                ) from error
        return {
            "request_id": str(uuid4()),
            "task": task,
            "provider": provider,
            "model": result.get("model") or model,
            "model_version": result.get("model_version"),
            "prompt_version": "synthetic-test-v1",
            "text": result["text"],
            "structured": structured,
            "usage": result.get("usage"),
            "latency_ms": round((time.monotonic() - start) * 1000),
            "warnings": result.get("warnings", []),
        }
