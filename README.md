The data attributes in the sample dataset (employee_attrition.csv) are as follows:

| **Data Attribute** | **Description** |
| :------------ |:--------------- |
| EmployeeID | Employee identity code |
| recorddate_key | Employee record date (Year of data) |
| birthdate_key | Employee birth date |
| orighiredate_key | Employee hired date |
| terminationdate_key | Employee termination date |
| age | Employee age |
| length_of_service | Length of service in year |
| city_name | City Name |
| department_name | Department Name |
| job_title | Job Title |
| store_name | Store Code |
| gender_short | Gender in short |
| gender_full | Gender in full |
| termreason_desc | Reason of termination |
| termtype_desc | Type of termination |
| STATUS_YEAR | Year of status |
| STATUS | Employee status in company |
| BUSINESS_UNIT | Business unit of employee |
  
  The data attributes after data pre-processing are as follows:

| **Column Number** | **Column**         | **Datatype** |
|:-------------------|:--------------------|:--------------|
| 1                 | EMPLOYEE_ID        | Factor       |
| 2                 | RECORD_DATE        | POSIXlt      |
| 3                 | BIRTH_DATE         | Date         |
| 4                 | ORI_HIRE_DATE      | Date         |
| 5                 | TERMINATION_DATE   | Date         |
| 6                 | AGE                | int          |
| 7                 | LENGTH_OF_SERVICE  | int          |
| 8                 | CITY               | Factor       |
| 9                 | DEPARTMENT         | Factor       |
| 10                | JOB_TITLE          | Factor       |
| 11                | STORE              | Factor       |
| 12                | GENDER             | Factor       |
| 13                | TERMINATION_REASON | Factor       |
| 14                | TERMINATION_TYPE   | Factor       |
| 15                | RECORD_YEAR        | int          |
| 16                | STATUS             | Factor       |
| 17                | BUSINESS_UNIT      | Factor       |
