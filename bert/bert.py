## FROM CHATGPT

import torch
from sentence_transformers import SentenceTransformer
from elasticsearch import Elasticsearch
import os
import json
import sys
import numpy as np

class BERT:
    def __init__(self):
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

        # Load BERT model and tokenizer
        self.model = SentenceTransformer('sentence-transformers/all-MiniLM-L6-v2')
        self.max_length = 512 - 2  # Accounting for [CLS] and [SEP] tokens

    def process_query(self, query):
        input_ids = self.tokenizer.encode(query, return_tensors='pt')
        with torch.no_grad():
            embeddings = self.model(input_ids)
        return embeddings

    def search_documents(self, query):
        response = self._es.search(index="notes", body={"query": {"match": {"content": query}}})
        for hit in response['hits']['hits']:
            print(hit['_source'])

    def search_embeddings(self, query_embedding):
        # Elasticsearch query to find similar embeddings
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
    
        # Execute the search query in the 'embeddings' index
        response = self._es.search(index="embeddings", body=search_query)
        
        # Extract document IDs from the search results
        document_ids = [hit["_id"] for hit in response["hits"]["hits"]]
        
        return document_ids
    
    def search_ids(self, doc_ids):
        # Initialize a list to hold the search results
        documents = []
        
        # Loop through each doc_id and fetch the corresponding document from the 'notes' index
        for doc_id in doc_ids:
            result = self._es.get(index="notes", id=doc_id)
            documents.append(result['_source'])  # Assuming the actual content is stored in the '_source' field
        
        return documents
    
    def natural_language_query(self, query):
        # Step 1: Generate embeddings from the query
        query_embeddings = self.generate_embeddings(query)
        
        # Step 2: Search for relevant document IDs using the embeddings
        doc_ids = self.search_embeddings(query_embeddings)
        
        # Step 3: Retrieve the top 10 relevant raw documents based on the document IDs
        # Assuming doc_ids are sorted by relevance, limit the search to the top 10
        relevant_docs = self.search_ids(doc_ids[:10])
        
        return relevant_docs

    def split_into_chunks(self, text, chunk_size=510):
        """
        Splits the text into chunks where each chunk has a maximum of chunk_size tokens.
        """
        # Initially split the text into paragraphs to preserve logical sections
        paragraphs = text.split('\n')

        chunks = []
        current_chunk = ""

        for paragraph in paragraphs:
            # Tokenize paragraph to count tokens
            paragraph_tokens = self.tokenizer.tokenize(paragraph)

            # Check if adding this paragraph exceeds the max_length
            if len(paragraph_tokens) + len(current_chunk.split()) <= chunk_size:
                current_chunk += paragraph + " "
            else:
                # Current chunk is full, add it to chunks and start a new one
                chunks.append(current_chunk.strip())
                current_chunk = paragraph + " "

            # Ensure the last chunk is added
            if current_chunk:
                chunks.append(current_chunk.strip())

        return chunks

    def tokenize_text(self, text):
        return self.tokenizer.encode(text, return_tensors='pt')

    def generate_embeddings(self, text):
        """# Split the text into chunks that are small enough for BERT processing
        chunks = self.split_into_chunks(text)
        all_embeddings = []
        
        for chunk in chunks:
            tokenized_chunk = self.tokenizer.encode_plus(chunk, max_length=512, truncation=True, padding='max_length', return_tensors='pt')
            with torch.no_grad():
                outputs = self.model(**tokenized_chunk)
            embeddings = outputs.last_hidden_state.mean(dim=1).numpy()[0]
            all_embeddings.append(embeddings)
        
        # Aggregate embeddings from all chunks
        # This is a simplistic aggregation method. Consider other approaches as needed.
        aggregated_embeddings = np.mean(all_embeddings, axis=0)
        
        return aggregated_embeddings"""

        # Generate embeddings for the input text (new model code)
        embeddings = self.model.encode(text)
        return embeddings
    
    def search_summary_documents(self):
        return