# DataMining
[금융데이터마이닝] 수업을 듣고 한 과제 코드 모음


과제는 모두 과제1, 과제2, 과제3, 과제4로 구성되었습니다.
1) 과제1 ~ 과제3까지 쓰인 데이터는 A123.csv로, 1500개 기업의 재무비율과 부도/비부도 여부(output 열에 있는 bank/nonbank)로 이루어져 있습니다. 
2) 과제4에 쓰인 데이터는 A4.csv로, 700개 기업의 재무비율만 나타나있고, 부도/비부도 여부는 데이터에 포함되어 있지 않습니다. 따라서 output열이 비워져 있어서 R 코드로 bank/nonbank를 예측하고 나타낼 수 있도록 합니다.

각 과제를 수행함에 있어 만들어진 코드의 내용 설명을 드리겠습니다.
과제1. 로지스틱 회귀분석 모형을 만들어 Confusion Matrix와 Accuracy 나타내었습니다.
과제2. 로지스틱 회귀분석 모형과 인공신경망 모형을 학습용(train)과 테스트용(test) 데이터로 나누어 만들고, 학습용 데이터와 테스트용 데이터에 대한 각각의 Confusion Matrix와 Accuracy를 나타내었습니다.
과제3. 의사결정나무 모형을 학습용(train)과 테스트용(test) 데이터로 나누어 만들고, 학습용 데이터와 테스트용 데이터에 대한 각각의 Confusion Matrix와 Accuracy를 나타내었습니다.
과제4. 위의 과제 1, 2, 3을 수행하며 만들어진 코드에 A4.csv 데이터를 적용시켜 부도/비부도를 예측하고 그 결과(bank/nonbank)를 csv 파일로 각각 저장하였습니다.
