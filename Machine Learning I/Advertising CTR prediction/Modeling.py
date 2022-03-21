# This is for simpler models (Logistic Regression and FeedForward Neural Net)


# Import the package

import numpy as np
import pandas as pd
import math
from tensorflow import keras
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation
from tensorflow.keras import optimizers
from tensorflow.keras.callbacks import EarlyStopping
import matplotlib.pyplot as plt
import category_encoders as ce
import tensorflow as tf
from tensorflow.keras import layers
from tensorflow.keras import initializers



# Preprocess the data

TrainData = pd.read_csv('ProjectTrainingData.csv',sep=',',header=0,quotechar='"')

# hour information

# The last two digits represent the hour of the time.
TrainData['day_hour'] = TrainData['hour'] % 100

# Check the digits: from 0 to 23
TrainData['day_hour'].unique()

# Plot the histogram of hour data
TrainData['day_hour'].hist()
plt.xticks(np.arange(0, 24, 1))
plt.show()

# Check the time of both ProjectTrainingData.csv and ProjectTestData.csv
SubData = pd.read_csv('ProjectTestData.csv',sep=',',header=0,quotechar='"')
SubData.sort_values(["hour"], ascending=True).head(20)
TrainData.sort_values(["hour"], ascending=False).head(20)

# We found out that the hour data contains '14102100' while our training data even include time '14102923'. 
# To avoid the time-series problem, I decide not to use RNN or 'hour', but consider the hour only.

# All the indepentdent variables in our dataset except the unique id.
variables = ['hour',
 'C1',
 'banner_pos',
 'site_id',
 'site_domain',
 'site_category',
 'app_id',
 'app_domain',
 'app_category',
 'device_id',
 'device_ip',
 'device_model',
 'device_type',
 'device_conn_type',
 'C14',
 'C15',
 'C16',
 'C17',
 'C18',
 'C19',
 'C20',
 'C21']



# Train-validation split

train_splits = []
val_splits = []

# We keep the click ratio same in both train and validaiton set.
for _, group_data in TrainData1.groupby("click"):
    random_selection = np.random.rand(len(group_data.index)) <= 0.6
    train_splits.append(group_data[random_selection])
    val_splits.append(group_data[~random_selection])

train_data1 = pd.concat(train_splits).sample(frac=1).reset_index(drop=True)
val_data1 = pd.concat(val_splits).sample(frac=1).reset_index(drop=True)

print(f"Train split size: {len(train_data.index)}")
print(f"Val split size: {len(val_data.index)}")



# Weight-of-Evidence encoding

ec = ce.WOEEncoder()

# Use the training set to generate encoding rules.
X = train_data1[variables]
X = X.fillna('NoData')
y = train_data1[['click']]

X.head()
X_WOE = ec.fit(X.to_numpy(), y)

# Transform the training set excluding the target variable
train_X = ec.transform(X.to_numpy())
train_X.round(4)

# New table = X + y variable
train_data = pd.concat([y.reset_index(drop=True), train_X], axis=1)
train_data.head()



# Data Transformation

## Transform the validation dataset
val_X = ec.transform(val_data1[variables].fillna('NoData').to_numpy()).round(4)
val_data = pd.concat([val_data1[['click']].reset_index(drop=True), val_X], axis=1)

## Transform the test dataset
sub_data = pd.read_csv('ProjectTestData.csv',sep=',',header=0,quotechar='"')
sub_data['day_hour'] = sub_data['hour'] % 100
sub_X = ec.transform(sub_data[variables].fillna('NoData').to_numpy()).round(4)
sub_X.head(5)



# Save the files

# We can direcly use these files afterward.
train_data_file = "train_data.csv"
val_data_file = "val_data.csv"
sub_data_file = "sub_data.csv"

train_data2.to_csv(train_data_file, index=False)
val_data.to_csv(val_data_file, index=False)
sub_X.to_csv(sub_data_file, index=False)



# Logistic Regression

# Read the data
train_data2 = pd.read_csv('train_data.csv',sep=',',header=0,quotechar='"')
val_data = pd.read_csv('val_data.csv',sep=',',header=0,quotechar='"')
test_data = pd.read_csv('sub_data.csv',sep=',',header=0,quotechar='"')

X_train = train_data2[variables]
y_train = train_data2[['click']]

X_val = val_data[variables]
y_val = val_data[['click']]

# Min-max scale
from sklearn.preprocessing import MinMaxScaler

scaler = MinMaxScaler(feature_range = (0,1))

scaler.fit(X_train)
X_train = scaler.transform(X_train)
X_val = scaler.transform(X_val)
test_data = scaler.transform(test_data)

# Train the model
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import log_loss

logisticRegr = LogisticRegression(solver = 'sag', n_jobs = -1)

logisticRegr.fit(X_train, y_train.values.ravel())

y_prob = logisticRegr.predict_proba(X_val)

log_loss(y_val, y_prob)

# Predict the submission data
y_sub = logisticRegr.predict_proba(test_data)
y_sub = pd.DataFrame(y_sub)
y_sub.to_csv('result1.csv', sep = ',', na_rep = 'NA', header = True, index = False)



# Simple Neural Network

# Read the data
# This time we tried letting the code read csv by itself.

def get_dataset_from_csv(csv_file_path, batch_size, shuffle=False):

    dataset = tf.data.experimental.make_csv_dataset(
        csv_file_path,
        batch_size=batch_size,
        column_names=CSV_READER,
        #column_defaults=COLUMN_DEFAULTS,
        label_name=TARGET_FEATURE_NAME,
        num_epochs=1,
        header=True,
        shuffle=shuffle,
    )
    return dataset.cache()

TARGET_FEATURE_NAME = "click"
TARGET_FEATURE_LABELS = ["0", "1"]
NUMERIC_FEATURE_NAMES = variables
FEATURE_NAMES = NUMERIC_FEATURE_NAMES
COLUMN_DEFAULTS = [
    [0.0] 
    for i in CSV_READER
]

NUM_CLASSES = 1



# Setup the model
learning_rate = 0.001
dropout_rate = 0.15
batch_size = 150000
num_epochs = 50

hidden_units = [32, 16]

train_data_file = "train_data.csv"
val_data_file = "val_data.csv"

# The code of setup is designed for embedding wide and deep neural network.
# The cited functions are on the official website of Keras (https://keras.io/examples/structured_data/wide_deep_cross_networks/)
# However, after trial, we found out that the problem is 'deep' in wide & deep NN depends on categorical variables,
# while we cannot use categories due to large number of levels, making the 'deep' makes no sense.
# Therefore, we can only run the baseline model.

def create_model_inputs():
    inputs = {}
    for feature_name in FEATURE_NAMES:
        if feature_name in NUMERIC_FEATURE_NAMES:
            inputs[feature_name] = layers.Input(
                name=feature_name, shape=(), dtype=tf.float32
            )
        else:
            inputs[feature_name] = layers.Input(
                name=feature_name, shape=(), dtype=tf.string
            )
    return inputs

from tensorflow.keras.layers import StringLookup

CATEGORICAL_FEATURE_NAMES = ['not exist']

def encode_inputs(inputs, use_embedding=False):
    encoded_features = []
    for feature_name in inputs:
        if feature_name in CATEGORICAL_FEATURE_NAMES:
            vocabulary = CATEGORICAL_FEATURES_WITH_VOCABULARY[feature_name]
            # Create a lookup to convert string values to an integer indices.
            # Since we are not using a mask token nor expecting any out of vocabulary
            # (oov) token, we set mask_token to None and  num_oov_indices to 0.
            lookup = StringLookup(
                vocabulary=vocabulary,
                mask_token=None,
                num_oov_indices=0,
                output_mode="int" if use_embedding else "binary",
            )
            if use_embedding:
                # Convert the string input values into integer indices.
                encoded_feature = lookup(inputs[feature_name])
                embedding_dims = int(math.sqrt(len(vocabulary)))
                # Create an embedding layer with the specified dimensions.
                embedding = layers.Embedding(
                    input_dim=len(vocabulary), output_dim=embedding_dims
                )
                # Convert the index values to embedding representations.
                encoded_feature = embedding(encoded_feature)
            else:
                # Convert the string input values into a one hot encoding.
                encoded_feature = lookup(tf.expand_dims(inputs[feature_name], -1))
        else:
            # Use the numerical features as-is.
            encoded_feature = tf.expand_dims(inputs[feature_name], -1)

        encoded_features.append(encoded_feature)

    all_features = layers.concatenate(encoded_features)
    return all_features

def create_baseline_model():
    inputs = create_model_inputs()
    features = encode_inputs(inputs)

    for units in hidden_units:
        features = layers.Dense(units, bias_initializer=initializers.Zeros())(features)
        features = layers.BatchNormalization()(features)
        features = layers.ReLU()(features)
        features = layers.Dropout(dropout_rate)(features)

    outputs = layers.Dense(units=1, activation="sigmoid")(features)
    model = keras.Model(inputs=inputs, outputs=outputs)
    return model


baseline_model = create_baseline_model()

model = baseline_model



# Train the model

model.compile(
    optimizer=keras.optimizers.Adam(learning_rate=learning_rate),
    loss=keras.losses.BinaryCrossentropy(),
    metrics=[keras.metrics.BinaryCrossentropy()],
)

train_dataset = get_dataset_from_csv(train_data_file, batch_size, shuffle=True)
#train_dataset.drop('index', axis = 1, inplace = True)

val_dataset = get_dataset_from_csv(val_data_file, batch_size)
#test_dataset.drop('index', axis = 1, inplace = True)

print("Start training the model...")

StopRule = EarlyStopping(monitor='val_loss', mode = 'min', verbose = 0, patience=100)

model.fit(train_dataset, validation_data = val_dataset, epochs=num_epochs, 
                    verbose = 0,callbacks = [StopRule])
print("Model training finished")

_, ll = model.evaluate(val_dataset, verbose=0)

print(f"Test ll: {round(ll * 100, 2)}%")



# Predict the submission data

sub_data = pd.read_csv('sub_data.csv',sep=',',header=0,quotechar='"')
sub_data = tf.data.experimental.make_csv_dataset(
	'sub_data.csv',
	batch_size=batch_size,
	column_names=FEATURE_NAMES,
	#column_defaults=COLUMN_DEFAULTS,
	#label_name=TARGET_FEATURE_NAME,
	num_epochs=1,
	header=True,
	shuffle=False,
)
sub_result = model.predict(sub_data)
sub_pred = pd.DataFrame(sub_result)
sub_pred.to_csv('result2.csv', sep = ',', na_rep = 'NA', header = True, index = False)

