import numpy as np
import tensorflow as tf
from tensorflow.keras.datasets import mnist
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Flatten

def load_local_data(file_path, file_type='csv'):
    """
    Load data from a local file
    
    Parameters:
        file_path (str): Path to the file
        file_type (str): Type of file ('csv', 'json', 'txt', or 'excel')
    
    Returns:
        Data from the file
    """
    import pandas as pd
    import json
    
    try:
        if file_type.lower() == 'csv':
            data = pd.read_csv(file_path)
            
        elif file_type.lower() == 'json':
            with open(file_path, 'r') as file:
                data = json.load(file)
                
        elif file_type.lower() == 'txt':
            with open(file_path, 'r') as file:
                data = file.read()
                
        elif file_type.lower() == 'excel':
            data = pd.read_excel(file_path)
            
        else:
            raise ValueError(f"Unsupported file type: {file_type}")
            
        return data
    
    except Exception as e:
        print(f"Error loading file: {str(e)}")
        return None

# Load dataset
(x_train, y_train), (x_test, y_test) = load_local_data(file_path, file_type='csv')

# Convert labels to one-hot encoding
y_train = tf.keras.utils.to_categorical(y_train, 10)
y_test = tf.keras.utils.to_categorical(y_test, 10)

# Split data into multiple clients
num_clients = 3
data_per_client = len(x_train) // num_clients

client_data = []
for i in range(num_clients):
    start = i * data_per_client
    end = start + data_per_client
    client_data.append((x_train[start:end], y_train[start:end]))

def create_model():
    model = Sequential([
        Dense(5, activation='sigmoid', input_shape=(28 * 28,)),  # One layer with 5 neurons
        Dense(10, activation='softmax')  # Output layer for 10 classes
    ])
    return model

global_model = create_model()
global_model.compile(optimizer='sgd', loss='categorical_crossentropy', metrics=['accuracy'])

def client_compute_gradients(client_model, client_data, global_weights):
    client_model.set_weights(global_weights)
    x_client, y_client = client_data
    with tf.GradientTape() as tape:
        predictions = client_model(x_client, training=True)
        loss = client_model.compiled_loss(y_client, predictions)
    gradients = tape.gradient(loss, client_model.trainable_variables)
    return gradients

# Function to aggregate gradients
def aggregate_gradients(gradients_list):
    avg_gradients = [
        np.mean([grad[client_idx].numpy() for grad in gradients_list], axis=0)
        for client_idx in range(len(global_model.trainable_variables))
    ]
    return avg_gradients

# FedSGD process
num_rounds = 500
learning_rate = 0.1

for round_num in range(num_rounds):
    print(f"Round {round_num + 1}")
    
    # Store gradients from all clients
    gradients_list = []
    for client_idx, (x_client, y_client) in enumerate(client_data):
        client_model = create_model()
        x_client = tf.constant(x_client)
        y_client = tf.constant(y_client)
        client_model.compile(optimizer='sgd', loss='categorical_crossentropy')
        gradients = client_compute_gradients(client_model, (x_client, y_client), global_model.get_weights())
        gradients_list.append(gradients)
    
    # Aggregate gradients
    avg_gradients = aggregate_gradients(gradients_list)
    
    # Update global model
    global_weights = global_model.get_weights()
    updated_weights = [
        global_weights[i] - learning_rate * avg_gradients[i]
        for i in range(len(global_weights))
    ]
    global_model.set_weights(updated_weights)

    # Evaluate global model
    loss, accuracy = global_model.evaluate(x_test, y_test, verbose=0)
    print(f"Global Model Loss: {loss:.4f}, Accuracy: {accuracy:.4f}")