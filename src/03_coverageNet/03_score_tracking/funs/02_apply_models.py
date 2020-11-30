import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow.keras import layers
from tensorflow.keras.models import load_model
import os

os.chdir("/Users/James/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt")

model_classification = load_model('pass_attempt_classification_model')

import tensorflow.keras.backend as K
def crps(y_true, y_pred):
    y_pred =  K.cumsum(y_pred, axis=1)
    ym =  K.cast(K.reshape(K.argmax(y_true, axis=1) - 99, (-1, 1)), 
        dtype='int32')
    n = K.arange(-99, 100)
    step = K.cast(K.greater_equal(n - ym, 0), dtype='float32')
    return K.mean(K.sum(K.square(y_pred - step), axis=1)) / 199

model_caught_yards = load_model('pass_caught_yards_model',
                               custom_objects={'crps':crps})

model_interception_yards = load_model('interceptions_yards_model',
                               custom_objects={'crps':crps})

# saving the classification data

# reading in the intermediate data
pass_arrived_score = pd.read_csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_tensor_score.csv")

pass_arrived_score2= pass_arrived_score.drop_duplicates(subset=['gameId', 'playId', 'targetNflId',
                                                                'frameId','defenseId'], keep='first')
pass_arrived_score2 = pass_arrived_score2.drop(['offense_nflId','offenseId','defense_offense_x',\
                                              'defense_offense_y', 'defense_offense_s_x', 'defense_offense_s_y'],
                                            axis = 1)


def create_x(data, nfeatures):
    # creating a numpy tensor to hold the play data
    np_mat = np.array(data.drop(['group', 'gameId', 'playId','targetNflId', 'frameId','defense_nflId'],
                               axis = 1))
    
    # setting parameters
    ngames = np.max(np.array(data['playId2']))
    nDef = 11
    
    # initializing the tensor
    np_tensor = np.zeros((ngames, nDef, nfeatures))
    
    for row in range(len(np_mat)):
        np_tensor[int(np_mat[row][0]) - 1][int(np_mat[row][1]) - 1] = np_mat[row][2:]
    
    np_tensor = np_tensor.astype('float32')
    
    return(np_tensor)

X_score = create_x(data = pass_arrived_score2,
                  nfeatures = 16)

score_preds = model_classification.predict(X_score)

full_preds_df = pd.DataFrame(score_preds, columns = ['IN_prob', 'I_prob', 'C_prob'])

full_preds_df2 = pd.concat([pass_arrived_score[["gameId", "playId", "targetNflId", "frameId"]].drop_duplicates().reset_index(), full_preds_df], axis = 1)
full_preds_df2 = full_preds_df2[full_preds_df2["gameId"] < 10000000000]

full_preds_df2 = full_preds_df2.drop(['index'], axis = 1)

full_preds_df2.to_csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_pass_attempt_classification_probs.csv", 
                        index = False)

# Saving Pass Caught Yards Data

# Formatting the Data
def create_x(data, nfeatures):
    # creating a numpy tensor to hold the play data
    np_mat = np.array(data.drop(['group', 'gameId', 'playId','targetNflId', 'frameId',
                                 'defense_nflId', 'offense_nflId'],
                               axis = 1))
    
    # setting parameters
    ngames = np.max(np.array(data['playId2']))
    nDef = 11
    nOff = 10
    
    # initializing the tensor
    np_tensor = np.zeros((ngames, nDef, nOff, nfeatures))
    
    for row in range(len(np_mat)):
        np_tensor[int(np_mat[row][0]) - 1][int(np_mat[row][1]) - 1][int(np_mat[row][2]) - 1] = np_mat[row][3:]
    
    np_tensor = np_tensor.astype('float32')
    
    return(np_tensor)

X_score = create_x(data = pass_arrived_score,
                  nfeatures = 20)

# Saving Predictions

score_preds = model_caught_yards.predict(X_score)


full_preds_df = pd.DataFrame(score_preds, columns = [str(int(x - 99)) for x in np.linspace(0,198, 199)])

full_preds_df2 = pd.concat([pass_arrived_score[["gameId","playId","targetNflId", "frameId"]].drop_duplicates().reset_index(), full_preds_df], axis = 1)
full_preds_df2 = full_preds_df2[full_preds_df2["gameId"] < 10000000000]
full_preds_df2 = full_preds_df2.drop(['index'], axis = 1)


full_preds_df3 = pd.melt(full_preds_df2, 
        id_vars=['gameId', 'playId', "targetNflId", "frameId"], 
        value_vars=[str(int(x - 99)) for x in np.linspace(0,198, 199)],
        var_name='offensePlayResult', 
        value_name='probability')

full_preds_df3["offensePlayResult"] = [int(x) for x in np.array(full_preds_df3["offensePlayResult"])]
full_preds_df3 = full_preds_df3.sort_values(['gameId','playId',"targetNflId", "frameId",'offensePlayResult'])

full_preds_df3.to_csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_pass_caught_yards_probs.csv", 
                        index = False)

# Saving Interceptions Dats

# Formatting the Data

def create_x(data, nfeatures):
    # creating a numpy tensor to hold the play data
    np_mat = np.array(data.drop(['group', 'gameId', 'playId','targetNflId', 'frameId',
                                 'defense_nflId', 'offense_nflId'],
                               axis = 1))
    
    # setting parameters
    ngames = np.max(np.array(data['playId2']))
    nDef = 11
    nOff = 10
    
    # initializing the tensor
    np_tensor = np.zeros((ngames, nDef, nOff, nfeatures))
    
    for row in range(len(np_mat)):
        np_tensor[int(np_mat[row][0]) - 1][int(np_mat[row][1]) - 1][int(np_mat[row][2]) - 1] = np_mat[row][3:]
    
    np_tensor = np_tensor.astype('float32')
    
    return(np_tensor)

X_score = create_x(data = pass_arrived_score,
                  nfeatures = 20)

# Saving Predictions

score_preds = model_interception_yards.predict(X_score)


full_preds_df = pd.DataFrame(score_preds, columns = [str(int(x - 99)) for x in np.linspace(0,198, 199)])

full_preds_df2 = pd.concat([pass_arrived_score[["gameId","playId","targetNflId", "frameId"]].drop_duplicates().reset_index(), full_preds_df], axis = 1)
full_preds_df2 = full_preds_df2[full_preds_df2["gameId"] < 10000000000]
full_preds_df2 = full_preds_df2.drop(['index'], axis = 1)


full_preds_df3 = pd.melt(full_preds_df2, 
        id_vars=['gameId', 'playId', "targetNflId", "frameId"], 
        value_vars=[str(int(x - 99)) for x in np.linspace(0,198, 199)],
        var_name='offensePlayResult', 
        value_name='probability')

full_preds_df3["offensePlayResult"] = [int(x) for x in np.array(full_preds_df3["offensePlayResult"])]
full_preds_df3 = full_preds_df3.sort_values(['gameId','playId',"targetNflId", "frameId",'offensePlayResult'])

full_preds_df3.to_csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_interceptions_yards_probs.csv", 
                        index = False)

del model_classification
del model_caught_yards
del model_interception_yards