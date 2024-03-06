from ingest import ESIngester
from bert import BERT
import requests

create = False
ingest = False
run = True

ingester = ESIngester()
ingester.setup_ingest()

if create:
    ingester.create_es_text_index("notes")
    ingester.create_es_vector_index("embeddings")

if ingest:
    for path_pdf in ingester.pdfs:
        ingester.ingest_pdf(path_pdf, "notes", "embeddings")

    for path_R in ingester.Rs:
        ingester.ingest_r_script(path_R, "notes", "embeddings")

    for path_doc in ingester.docs:
        ingester.ingest_docx(path_doc, "notes", "embeddings")

if run:
    if __name__ == "__main__":
        bert_model = BERT()  # Initialize your BERT model class

        print("Welcome to the interactive BERT-based search. Type 'exit' to quit.")
        while True:
            user_query = input("Enter your query: ")
            if user_query.lower() == 'exit':
                print("Exiting the interactive search.")
                break
            relevant_docs = bert_model.natural_language_query(user_query)

            # Concatenate the content of the top 3 relevant documents
            concatenated_context = " ".join([doc['content'] for doc in relevant_docs])  # Assuming each doc has a 'content' field

            # Prepare data for POST request
            data = {'context': concatenated_context, 'query': user_query}
            
            # Send POST request to the LLM service
            response = requests.post('http://ragllm-llm-1:5000/answer', json=data)
            
            if response.status_code == 200:
                answer = response.json()['answer']
                print(f"Answer: {answer}\n")
                print(f"Context: {data['context']}")
            else:
                print("Failed to get an answer from the LLM service.")