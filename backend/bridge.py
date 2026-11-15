"""Single-request stdin/stdout bridge; never opens a network listener."""

import asyncio
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from copilot_adapter import CopilotAdapter
from model_gateway import GatewayError, ModelGateway


async def dispatch(request, gateway):
    if not isinstance(request, dict) or request.get("synthetic_confirmed") is not True:
        raise GatewayError(
            "Confirm that the input is entirely synthetic before connecting."
        )
    provider = request.get("provider", "")
    if request.get("action") == "models":
        return {"models": await gateway.models(provider)}
    if request.get("action") == "generate":
        return await gateway.generate(
            provider,
            request.get("model", ""),
            "synthetic_test",
            request.get("prompt", ""),
            request.get("schema"),
        )
    raise GatewayError("Unknown gateway action.")


def main():
    try:
        request = json.load(sys.stdin)
        result = asyncio.run(
            dispatch(request, ModelGateway({"copilot": CopilotAdapter()}))
        )
        response = {"ok": True, "result": result}
    except (GatewayError, ValueError, TypeError) as error:
        response = {"ok": False, "error": str(error)}
    except Exception:
        response = {
            "ok": False,
            "error": "Unexpected backend error; check local configuration.",
        }
    sys.stdout.write(json.dumps(response, ensure_ascii=True))


if __name__ == "__main__":
    main()
