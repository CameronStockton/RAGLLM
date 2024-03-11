## FROM CHATGPT

import torch
from sentence_transformers import SentenceTransformer, InputExample, losses
from elasticsearch import Elasticsearch
import os
import json
import sys
import numpy as np
from sklearn.model_selection import train_test_split
from torch.utils.data import DataLoader


class BERT:
    def __init__(self):
        """ Initialize the BERT Class with Elasticsearch and the model from HF"""
        ES_HOSTS = json.loads(os.environ.get('ES_HOSTS', '["https://ragllm-es01-1:9200", "https://ragllm-es02-1:9200", "https://ragllm-es03-1:9200"]'))
        ES_CA_CERTS = os.environ.get('ES_CA_CERTS', '/usr/share/elasticsearch/config/certs/ca/ca.crt')
        ES_HTTP_AUTH = (
            os.environ.get('ES_USER', 'elastic'), 
            os.environ.get('ES_PASSWORD', 'laylabakaa')
        )

        # Initialize Elasticsearch
        self._es = Elasticsearch(
            hosts=ES_HOSTS,
            verify_certs=True,
            ca_certs=ES_CA_CERTS,
            http_auth=ES_HTTP_AUTH
        )

        # Load BERT model
        self.model = SentenceTransformer('sentence-transformers/all-MiniLM-L6-v2')
        self.max_length = 512 - 2  # Accounting for [CLS] and [SEP] tokens

    def search_embeddings(self, query_embedding, index="embeddings", k=10):
        """Using a Query Embedding, search Elasticsearch to find similar embeddings"""
        search_query = {
            "query": {
                "script_score": {
                    "query": {"match_all": {}},
                    "script": {
                        "source": "cosineSimilarity(params.query_vector, 'embeddings') + 1.0",
                        "params": {"query_vector": query_embedding}
                    }
                }
            }
        }

        """search_query = {
                "size": k,
                "query": {
                    "knn": {
                        "embeddings": {
                            "vector": query_embedding,
                            "k": k
                        }
                    }
                }
            }"""
    
        # Execute the search query in the index (default is embeddings)
        response = self._es.search(index=index, body=search_query)
        
        # Extract document IDs from the search results -> to be used for search_ids
        document_ids = [hit["_id"] for hit in response["hits"]["hits"]]
        
        return document_ids
    
    def search_ids(self, doc_ids, index="notes"):
        """Given Document IDs, search the raw data index for the results"""
        # Initialize a list to hold the search results
        documents = []
        
        # Loop through each doc_id and fetch the corresponding document from the 'notes' index
        for doc_id in doc_ids:
            result = self._es.get(index=index, id=doc_id)
            documents.append(result['_source'])  # Assuming the actual content is stored in the '_source' field
        
        return documents
    
    def natural_language_query(self, query, raw_index, vec_index):
        """Uses previous helper functions to generate relevant documents for a users natural language query"""
        # Step 1: Generate embeddings from the query
        query_embeddings = self.generate_embeddings(query)
        
        # Step 2: Search for relevant document IDs using the embeddings
        doc_ids = self.search_embeddings(query_embeddings, vec_index)
        
        # Step 3: Retrieve the top 5 relevant raw documents based on the document IDs
        # Assuming doc_ids are sorted by relevance, limit the search to the top 5
        #NEW STEP 3: Just retrieve top document
        relevant_docs = self.search_ids(doc_ids[:1], raw_index)
        
        return relevant_docs

    def generate_embeddings(self, text):
        """Generate embeddings for the input text"""
        embeddings = self.model.encode(text)
        return embeddings
    
    def load_json_data(self, json_filepath):
        with open(json_filepath, 'r') as file:
            data = json.load(file)
        return data
    
    def fine_tune(self, training_data=None, model_save_path='./fine_tuned_model/'):
        """
        Fine-tune the SentenceTransformer model.
        training_data: A list of dictionaries with 'query', 'context', and 'context_rating'.
        model_save_path: Path to save the fine-tuned model.
        """
        if not training_data:
            training_data = self.load_json_data('./logs/answer_log.json')
        # Convert training data to a format suitable for SentenceTransformer
        train_examples = []
        for item in training_data:
            score = float(item['context_rating'])
            train_examples.append(InputExample(texts=[item['query'], item['context']], label=score))
        
        # Convert the examples to a DataLoader
        train_dataloader = DataLoader(train_examples, shuffle=True, batch_size=16)
        
        # Use a suitable loss function. CosineSimilarityLoss is commonly used for similarity tasks.
        train_loss = losses.CosineSimilarityLoss(model=self.model)
        
        # Fine-tune the model
        self.model.fit(train_objectives=[(train_dataloader, train_loss)], epochs=1, warmup_steps=100)
        
        # Save the fine-tuned model
        self.model.save(model_save_path)

    def log_answer(self, data):
        """ Log the answer in a structured JSON file for training purposes
            This is duplicate to the log_answer function found in the llm container.
            We can replace this function by creating a shared volume. """
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