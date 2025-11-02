import { Tool } from "@modelcontextprotocol/sdk/types.js";

export interface ToolHandler {
  (args: Record<string, unknown>): Promise<string>;
}

export interface ToolDefinition {
  tool: Tool;
  handler: ToolHandler;
}

export * from "./types/github.js";
