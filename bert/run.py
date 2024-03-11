from ingest import ESIngester
from bert import BERT
import requests
from bell.applications_template import app_template

create = False
ingest = False
fine_tune = False
run = True
bell = False

ingester = ESIngester()
ingester.setup_ingest()

if create:
    ingester.create_es_text_index("notes")
    ingester.create_es_vector_index("embeddings")
    ingester.create_es_text_index("app_raw")
    ingester.create_es_vector_index("app_vec")

if ingest:
    for path_pdf in ingester.pdfs:
        ingester.ingest_pdf(path_pdf, "notes", "embeddings")

    for path_R in ingester.Rs:
        ingester.ingest_r_script(path_R, "notes", "embeddings")

    for path_doc in ingester.docs:
        ingester.ingest_docx(path_doc, "notes", "embeddings")

if bell:
    ingester.natural_language_from_template('./bell/applications.json', app_template, "app_raw", "app_vec")

if fine_tune:
    model = BERT()
    model.fine_tune()

if run:
    if __name__ == "__main__":
        bert_model = BERT()

        print("Welcome to the interactive BERT-based search. Type 'exit' to quit.")
        while True:
            user_query = input("Enter your query: ")
            if user_query.lower() == 'exit':
                print("Exiting the interactive search.")
                break
            relevant_docs = bert_model.natural_language_query(user_query, "app_raw", "app_vec")

            concatenated_context = " ".join([doc['content'] for doc in relevant_docs])  # Assuming each doc has a 'content' field

            data = {'context': concatenated_context, 'query': user_query}
            
            # Send POST request to the LLM service
            response = requests.post('http://ragllm-llm-1:5000/answer', json=data)
            
            if response.status_code == 200:
                answer = response.json()['answer']
                print(f"Answer: {answer}\n")
                answer_rating = input("Rate the answer accuracy (0 to 1): ")
                print(f"Context: {data['context']}")
                context_rating = input("Rate the context relevance (0 to 1): ")
                #Log request for training feedback
                logged_answer = [data["query"], data["context"], answer, answer_rating, context_rating]
                bert_model.log_answer(logged_answer)
                requests.post('http://ragllm-llm-1:5000/log-answer', json=logged_answer)
            else:
                print("Failed to get an answer from the LLM service.")