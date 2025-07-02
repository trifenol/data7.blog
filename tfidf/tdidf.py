import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import matplotlib.pyplot as plt
import seaborn as sns
import math
from collections import Counter
import string
import unicodedata

# Documentos de exemplo
documentos = [
    "Python é uma linguagem de programação poderosa e versátil",
    "Machine learning utiliza algoritmos para aprender padrões nos dados",
    "Análise de dados com Python é muito eficiente e popular",
    "Deep learning é uma subcategoria do machine learning",
    "Visualização de dados ajuda na interpretação de resultados",
    "Pandas é uma biblioteca Python essencial para análise de dados"
]

labels = ['Python', 'ML', 'Análise', 'Deep Learning', 'Visualização', 'Pandas']

def preprocessar_texto(texto):
    """Pré-processa texto para melhorar resultados do TF-IDF"""
    texto = texto.lower()
    texto = unicodedata.normalize('NFD', texto)
    texto = ''.join(char for char in texto if unicodedata.category(char) != 'Mn')
    texto = texto.translate(str.maketrans('', '', string.punctuation))
    texto = ''.join(char for char in texto if not char.isdigit())
    return texto

# Implementação com scikit-learn
vectorizer = TfidfVectorizer(
    max_features=100,
    stop_words=None,
    ngram_range=(1, 1),
    lowercase=True,
    token_pattern=r'\b[a-zA-ZÀ-ÿ]+\b'
)

tfidf_matrix = vectorizer.fit_transform(documentos)
feature_names = vectorizer.get_feature_names_out()
tfidf_dense = tfidf_matrix.toarray()

df_tfidf = pd.DataFrame(
    tfidf_dense,
    columns=feature_names,
    index=labels
)

# Exibindo resultados
print("Top 5 palavras mais importantes por documento:")
for i, doc_label in enumerate(labels):
    top_scores = df_tfidf.iloc[i].nlargest(5)
    print(f"\n{doc_label}:")
    for palavra, score in top_scores.items():
        print(f"  {palavra}: {score:.4f}")

# Implementação manual do TF-IDF
def calcular_tf(documento):
    palavras = documento.lower().split()
    total_palavras = len(palavras)
    contador = Counter(palavras)

    tf = {}
    for palavra, freq in contador.items():
        tf[palavra] = freq / total_palavras
    return tf

def calcular_idf(documentos):
    N = len(documentos)
    todas_palavras = set()

    for doc in documentos:
        palavras = set(doc.lower().split())
        todas_palavras.update(palavras)

    idf = {}
    for palavra in todas_palavras:
        docs_com_palavra = sum(1 for doc in documentos if palavra in doc.lower().split())
        idf[palavra] = math.log(N / docs_com_palavra)

    return idf

def calcular_tfidf_manual(documentos):
    idf_scores = calcular_idf(documentos)
    tfidf_docs = []

    for doc in documentos:
        tf_scores = calcular_tf(doc)
        tfidf_doc = {}

        for palavra, tf in tf_scores.items():
            tfidf_doc[palavra] = tf * idf_scores[palavra]

        tfidf_docs.append(tfidf_doc)

    return tfidf_docs

# Análise de similaridade
similarity_matrix = cosine_similarity(tfidf_matrix)
df_similarity = pd.DataFrame(
    similarity_matrix,
    index=labels,
    columns=labels
)

def encontrar_similares(doc_index, top_n=2):
    similarities = df_similarity.iloc[doc_index].sort_values(ascending=False)
    similares = similarities[1:top_n+1]

    print(f"Documentos mais similares a '{labels[doc_index]}':")
    for doc, score in similares.items():
        print(f"  {doc}: {score:.4f}")

# Visualizações
top_words = df_tfidf.max().nlargest(15).index
df_viz = df_tfidf[top_words]

plt.figure(figsize=(12, 8))
sns.heatmap(
    df_viz.T,
    annot=True,
    fmt='.3f',
    cmap='YlOrRd',
    cbar_kws={'label': 'TF-IDF Score'}
)
plt.title('TF-IDF Scores - Top 15 Palavras por Documento')
plt.xlabel('Documentos')
plt.ylabel('Palavras')
plt.tight_layout()
plt.show()

plt.figure(figsize=(10, 8))
sns.heatmap(
    df_similarity,
    annot=True,
    fmt='.3f',
    cmap='coolwarm',
    center=0,
    square=True
)
plt.title('Similaridade Coseno entre Documentos (baseada em TF-IDF)')
plt.tight_layout()
plt.show()

# Exemplo de uso
tfidf_manual = calcular_tfidf_manual(documentos)
encontrar_similares(0)