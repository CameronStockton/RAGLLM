from flask import Flask, request, jsonify
from transformers import pipeline

app = Flask(__name__)

class LLM:
    def __init__(self):
        self.model = pipeline("question-answering", model="deepset/roberta-base-squad2")

    def answer_question(self, context, question):
        return self.model(question=question, context=context)

llm = LLM()

@app.route('/answer', methods=['POST'])
def answer():
    data = request.get_json()
    context = data['context']
    question = data['query']
    response = llm.answer_question(context, question)
    return jsonify(response)

if __name__ == "__main__":
    app.run(debug=True, host='0.0.0.0', port=5000)