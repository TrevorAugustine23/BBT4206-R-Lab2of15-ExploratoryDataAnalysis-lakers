Business Intelligence Lab Submission Markdown
================
<Lakers>
\<26/09/2023\>

- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
- [Loading the Student Performance
  Dataset](#loading-the-student-performance-dataset)
  - [Description of the Dataset](#description-of-the-dataset)
- [We find Kurtosis of each
  variable](#we-find-kurtosis-of-each-variable)
  - [ANOVA\>](#anova)
  - [\<You Can Have Another Sub-Title Here if you
    wish\>](#you-can-have-another-sub-title-here-if-you-wish)

# Student Details

|                                                   |                                                                                                                                                                               |              |
|---------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------|
| **Student ID Numbers and Names of Group Members** | 1\. 135203 - C - Tom Arnold \| \| 2. 131749 - C - Teresia Nungari \| \| 3. 134780 - C - Trevor Augustine \| 4. 132840 - C - Sheila Wangui \| \| 5. ID - Class Group - Name \| |              |
|                                                   | **GitHub Classroom Group Name**                                                                                                                                               | *\<Lakers\>* |
| **Course Code**                                   | BBT4206                                                                                                                                                                       |              |
| **Course Name**                                   | Business Intelligence II                                                                                                                                                      |              |
| **Program**                                       | Bachelor of Business Information Technology                                                                                                                                   |              |
| **Semester Duration**                             | 21<sup>st</sup> August 2023 to 28<sup>th</sup> November 2023                                                                                                                  |              |

# Setup Chunk

We start by installing all the required packages

``` r
## formatR - Required to format R code in the markdown ----
if (!is.element("formatR", installed.packages()[, 1])) {
  install.packages("formatR", dependencies = TRUE,
                   repos="https://cloud.r-project.org")
}
require("formatR")


## readr - Load datasets from CSV files ----
if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE,
                   repos="https://cloud.r-project.org")
}
require("readr")
```

------------------------------------------------------------------------

**Note:** the following “*KnitR*” options have been set as the defaults
in this markdown:  
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy.opts = list(width.cutoff = 80), tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

``` r
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    warning = FALSE,
    collapse = FALSE,
    tidy = TRUE
)
```

------------------------------------------------------------------------

**Note:** the following “*R Markdown*” options have been set as the
defaults in this markdown:

> output:  
>   
> github_document:  
> toc: yes  
> toc_depth: 4  
> fig_width: 6  
> fig_height: 4  
> df_print: default  
>   
> editor_options:  
> chunk_output_type: console

# Loading the Student Performance Dataset

The 20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset is then
loaded. The dataset and its metadata are available here:
<https://drive.google.com/drive/folders/1-BGEhfOwquXF6KKXwcvrx7WuZXuqmW9q?usp=sharing>

`{library(readr)} library(readr) Student <- read_csv(   "data/Student.csv",   col_types = cols(     class_group = col_factor(levels = c("A","B","C")),     gender = col_factor(levels = c("0","1")),     regret_choosing_bi = col_factor(levels = c("0","1")),     drop_bi_now = col_factor(levels = c("0","1")),     motivator = col_factor(levels = c("0","1")),     read_content_before_lecture = col_factor(levels = c("0","1","2","3","4","5")),     anticipate_test_questions = col_factor(levels = c("0","1","2","3","4","5")),     answer_rhetorical_questions = col_factor(levels = c("0","1","2","3","4","5")),     copy_new_terms_in_reading_notebook = col_factor(levels = c("0","1","2","3","4","5")),     take_quizzes_and_use_results = col_factor(levels = c("0","1","2","3","4","5")),     read_content_before_lecture = col_factor(levels = c("0","1","2","3","4","5")),     reorganise_course_outline = col_factor(levels = c("0","1","2","3","4","5")),     write_down_important_points = col_factor(levels = c("0","1","2","3","4","5")),     space_out_revision = col_factor(levels = c("0","1","2","3","4","5")),     studying_in_study_group = col_factor(levels = c("0","1","2","3","4","5")),     schedule_appointments = col_factor(levels = c("0","1","2","3","4","5")),     goal_oriented = col_factor(levels = c("0","1")),     spaced_repetition = col_factor(levels = c("0","1","2","3","4","5")),     testing_and_active_recall = col_factor(levels = c("0","1","2","3","4","5")),     interleaving = col_factor(levels = c("0","1","2","3","4","5")),     categorizing = col_factor(levels = c("0","1","2","3","4","5")),     retrospective_timetable = col_factor(levels = c("0","1","2","3","4","5")),     cornell_notes = col_factor(levels = c("0","1","2","3","4","5")),     sq3r = col_factor(levels = c("0","1","2","3","4","5")),     commute = col_factor(levels = c("0","1","2","3","4","5")),     study_time = col_factor(levels = c("0","1","2","3","4","5")),     repeats_since_Y1 = col_integer(),     paid_tuition = col_factor(levels = c("0","1")),     free_tuition = col_factor(levels = c("0","1")),     extra_curricular = col_factor(levels = c("0","1")),     sports_extra_curricular = col_factor(levels = c("0","1")),     exercise_per_week = col_integer(),     meditate = col_integer(),     pray = col_integer(),     internet = col_factor(levels = c("0","1")),     laptop = col_factor(levels = c("0","1")),     family_relationships = col_factor(levels = c("0","1","2","3","4","5")),     friendships = col_factor(levels = c("0","1","2","3","4","5")),     romantic_relationships = col_factor(levels = c("0","1","2","3","4","5")),     spiritual_wellnes = col_factor(levels = c("0","1","2","3","4","5")),     financial_wellness = col_factor(levels = c("0","1","2","3","4","5")),     health = col_factor(levels = c("0","1","2","3","4","5")),     day_out = col_integer(),     night_out = col_integer(),     alcohol_or_narcotics = col_factor(levels = c("0","1")),     mentor = col_factor(levels = c("0","1")),     mentor_meetings = col_integer()   )) View(Student)`

## Description of the Dataset

We then display the number of instances in each class.

Student_class_group_freq \<- Student\$class_group cbind(frequency =
table(Student_class_group_freq), percentage =
prop.table(table(Student_class_group_freq)) \* 100)

Student_gender_freq \<- Student\$gender cbind(frequency =
table(Student_gender_freq), percentage =
prop.table(table(Student_gender_freq)) \* 100)

Student_regret_choosing_bi_freq \<- Student\$regret_choosing_bi
cbind(frequency = table(Student_regret_choosing_bi_freq), percentage =
prop.table(table(Student_regret_choosing_bi_freq)) \* 100)

Student_drop_bi_now_freq \<- Student\$regret_choosing_bi cbind(frequency
= table(Student_drop_bi_now_freq), percentage =
prop.table(table(Student_drop_bi_now_freq)) \* 100)

Student_motivator_freq \<- Student\$motivator cbind(frequency =
table(Student_motivator_freq), percentage =
prop.table(table(Student_motivator_freq)) \* 100)


    Next we discover the mode of different variables

    `Student_class_group_mode <- names(table(Student$class_group))[
      which(table(Student$class_group) == max(table(Student$class_group)))
    ]
    print(Student_class_group_mode)

    Student_gender_mode <- names(table(Student$gender))[
      which(table(Student$gender) == max(table(Student$gender)))
    ]
    print(Student_gender_mode)

    Student_regret_choosing_bi_mode <- names(table(Student$regret_choosing_bi))[
      which(table(Student$regret_choosing_bi) == max(table(Student$regret_choosing_bi)))
    ]
    print(Student_regret_choosing_bi_mode)

    Student_drop_bi_now_mode <- names(table(Student$drop_bi_now))[
      which(table(Student$drop_bi_now) == max(table(Student$drop_bi_now)))
    ]
    print(Student_drop_bi_now_mode)

    Student_motivator_mode <- names(table(Student$motivator))[
      which(table(Student$motivator) == max(table(Student$motivator)))
    ]
    print(Student_motivator_mode)

# We find Kurtosis of each variable

Describe the code chunk here:

\`\`if (!is.element(“e1071”, installed.packages()\[, 1\])) {
install.packages(“e1071”, dependencies = TRUE) } require(“e1071”)
sapply(Student\[, 10\], kurtosis, type = 2) sapply(Student\[, 33\],
kurtosis, type = 2)

## ANOVA\>

Student_one_way_anova \<- aov( YOB ~ study_time, data = Student)
summary(Student_one_way_anova)

\#Histogram Student_YOB \<- as.numeric(unlist(Student\[, 4\]))
hist(Student_YOB, main = names(Student)\[4\])

## \<You Can Have Another Sub-Title Here if you wish\>

``` r
# Fill this with other R related code that will be executed when the R markdown
```

**etc.** as per the lab submission requirements. Be neat and communicate
in a clear and logical manner.
