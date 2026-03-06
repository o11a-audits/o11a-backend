use serde::{Deserialize, Serialize};

const LARGE_MODEL: &str = "anthropic/claude-opus-4.6";
const SMALL_MODEL: &str = "z-ai/glm-5";

const SYSTEM_MESSAGE: &str = "\
You are an expert smart contract security auditor. \
Analyze the provided Solidity smart contract code with extreme rigor and precision. \
Adherance to documentation is critical — verify every assertion, invariant, and contract requirement based on the documentation. \
Consider how each piece of code interacts with the broader documented system, \
paying close attention to access control, reentrancy, integer arithmetic, \
external calls, state mutations, and protocol-level logic. \
Do not gloss over details — if something is subtle or ambiguous, call it out. \
Be thorough but concise in your response.";

pub enum TaskSize {
  Large,
  Small,
}

#[derive(Debug, Serialize)]
struct ChatMessage {
  role: &'static str,
  content: String,
}

#[derive(Debug, Deserialize)]
struct ChatCompletionResponse {
  choices: Vec<Choice>,
}

#[derive(Debug, Deserialize)]
struct Choice {
  message: ResponseMessage,
}

#[derive(Debug, Deserialize)]
struct ResponseMessage {
  content: String,
}

/// Send a prompt to the OpenRouter API with the audit system message prepended.
pub async fn chat_completion(
  task_size: TaskSize,
  prompt: &str,
) -> Result<String, String> {
  let api_key = std::env::var("OPENROUTER_API_KEY").map_err(|_| {
    "OPENROUTER_API_KEY environment variable not set".to_string()
  })?;

  let model = match task_size {
    TaskSize::Large => LARGE_MODEL,
    TaskSize::Small => SMALL_MODEL,
  };

  let messages = vec![
    ChatMessage {
      role: "system",
      content: SYSTEM_MESSAGE.to_string(),
    },
    ChatMessage {
      role: "user",
      content: prompt.to_string(),
    },
  ];

  let body = serde_json::json!({
    "model": model,
    "messages": messages,
  });

  let client = reqwest::Client::new();
  let response = client
    .post("https://openrouter.ai/api/v1/chat/completions")
    .header("Authorization", format!("Bearer {}", api_key))
    .json(&body)
    .send()
    .await
    .map_err(|e| format!("Request failed: {}", e))?;

  if !response.status().is_success() {
    let status = response.status();
    let body = response.text().await.unwrap_or_default();
    return Err(format!("API error ({}): {}", status, body));
  }

  let parsed: ChatCompletionResponse = response
    .json()
    .await
    .map_err(|e| format!("Failed to parse response: {}", e))?;

  parsed
    .choices
    .into_iter()
    .next()
    .map(|c| c.message.content)
    .ok_or_else(|| "No choices in response".to_string())
}
