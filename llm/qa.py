from flask import Flask, request, jsonify
from transformers import pipeline
import json

app = Flask(__name__)


class LLM:
    def __init__(self):
        """Initialize the LLM model"""
        access_token = "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"
        self.model = pipeline("question-answering", model="deepset/roberta-base-squad2")
        #self.model = pipeline("text-generation", model="meta-llama/Llama-2-7b-chat-hf", token = access_token)

    def answer_question(self, context, question):
        """Answer a user's question in natural language"""
        return self.model(question=question, context=context)
        """prompt = f"Context: {context}\nQuestion: {question}\nAnswer:"
        # Since this is a text-generation model, might want to tweak generation parameters like max_length
        response = self.model(prompt, max_length=512, num_return_sequences=1)
        # Assuming the best answer is the first one returned by the model, but can look at others
        return {"answer": response[0]['generated_text']}"""

llm = LLM()

@app.route('/answer', methods=['POST'])
def answer():
    data = request.get_json()
    context = data['context']
    question = data['query']
    response = llm.answer_question(context, question)
    return jsonify(response)

@app.route('/log-answer', methods=['POST'])
def log_answer(): 
    data = request.json
    structured_data = {
        "query": data[0],
        "context": data[1],
        "answer": data[2],
        "answer_rating": data[3],
        "context_rating": data[4]
    }
    with open('./logs/answer_log.jsonl', 'a') as log_file:
        json.dump(structured_data, log_file)
        log_file.write('\n')
    return 'Logged', 200

if __name__ == "__main__":
    app.run(debug=True, host='0.0.0.0', port=5000)