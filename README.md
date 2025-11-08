# 운동 여부가 심혈관 질환에 미치는 영향 분석 (R)

Kaggle의 Cardiovascular Disease dataset을 활용하여 운동 여부가 심혈관 질환 발생에 미치는 영향을 통계적으로 분석한 프로젝트입니다.

---

## 💾 데이터

* **출처:** Kaggle (Cardiovascular Disease dataset)
* **링크:** [https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset](https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset)
* **참고:** 원본 데이터(`.csv`)는 용량 문제로 `.gitignore`에 의해 이 저장소에 포함되지 않았습니다. 위 링크에서 데이터를 다운로드할 수 있습니다.

---

## 📊 주요 분석 내용


* Kaggle의 Cardiovascular Disease dataset을 활용하여 운동 여부가 심혈관 질환 발생의 미치는 영향을 통계적으로 분석하였습니다.
* 이를 위해 공변량의 분포를 비교하였습니다.
* 로지스틱 회귀 분석(Logistic Regression)을 통해 운동 여부의 효과를 추정하였습니다.
* 교호작용 및 모델의 예측력을 AUC 지표를 통해 추가적으로 검토하였습니다.

---

## 📈 핵심 결과 (예시)

* (여기에 분석 후 발견한 핵심 인사이트를 1~2줄로 요약)
* *예: 로지스틱 회귀 분석 결과, 운동 여부는 심혈관 질환 발생 확률을 약 XX% 낮추는 것으로 나타났습니다. (Odds Ratio = 0.XX)*
* *예: 개발된 모델의 예측 정확도(AUC)는 0.XX로 준수한 성능을 보였습니다.*

---

## 🛠️ 사용 기술

* **Language:** R
* **IDE:** RStudio
* **주요 패키지:**
    * `(데이터 처리에 쓴 패키지, 예: dplyr)`
    * `(시각화에 쓴 패키지, 예: ggplot2)`
    * `(모델링에 쓴 패키지, 예: pROC 등)`