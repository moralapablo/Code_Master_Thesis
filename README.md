# Code_Master_Thesis_Pablo_Morala

This repository contains all the code needed to implement the formulas, the simulations and the plots  used in the following Master Thesis:

* **Title**: "Polynomial regression as a proxy for interpretability of Neural Networks"
* **Author :** Pablo Morala Miguélez
* **Supervisors:** Iñaki Úcar Marqués, Jenny Alexandra Cifuentes, Rosa E. Lillo Rodríguez 
* **University**: Universidad Carlos III de Madrid (UC3M)
* **Date:** July 2020
* **Keywords**: Neural Networks, Polynomial Regression, Interpretability, Activation Functions, Machine Learning.

### Summary

Neural Networks, even when being a really powerful tool in Data Science, remain being considered "black boxes" due to their often opaque nature. This limitation has attracted a world-wide interest to achieve an explanation and understanding of the model decisions. Furthermore, tuning their hyperparameters and structure has not a general approach and depends on each application. In an attempt to solve those problems, the equivalence between Neural Networks and Polynomial Regression has been proposed recently.

In this Master's Thesis, we explore this idea by trying to build an explicit expression for the coefficients of an equivalent Polynomial Regression from the weights of a given Neural Network, using a Taylor expansion approach. This is achieved for single hidden layer Neural Networks for regression problems. Then we empirically test the performance of our method and discuss how it could be extended to more general Neural Networks.
