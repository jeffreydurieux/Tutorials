#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 28 11:32:45 2019

@author: jeffreydurieux
"""

# This python script contains a function to compute the modified RV coefficient

# # reference: Smilde AK, Kiers HAL, Bijlsma S, Rubingh CM, van Erk MJ (2009) 
# Matrix correlations for high-dimen- sional data: the modified RV-coefficient. 
# Bioinformatics 25(3):401–405


import numpy as np


def modRV(X, Y):
    '''
    This functions computes the modified-RV coefficient
    Input: two numpy arrays
    Output: numpy array with a single number
    '''
    
    XXtilde = ( np.matmul(X, X.T)) - np.diag(np.diag(np.matmul(X, X.T)))
    YYtilde = ( np.matmul(Y, Y.T)) - np.diag(np.diag(np.matmul(Y, Y.T)))
    
    Xvec = np.matrix(np.concatenate(XXtilde))
    Yvec = np.matrix(np.concatenate(YYtilde))
    
    
    
    Res = (np.dot(Xvec,Yvec.T))  /  ( np.sqrt( (np.dot(Xvec,Xvec.T)) * (np.dot(Yvec,Yvec.T)) ) )
    
    return(np.array(Res))
    
    