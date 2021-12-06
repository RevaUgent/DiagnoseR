##-------------- INFO SHEET
InfoUI <- function() {
  tagList(
      h2("BackpocketPhysio project"),
      HTML(
      "<p>The BackpocketPhysio project tries to translate science within the Physiotherapist to the daily practice.
      The current BackpocketPhysio › Diagnosis APP aims to set-up a datbase that includes all diagnostics tests that are
      applied in a physiotherapy practice. The dataset with all information can also be downloaded from the website,
      and is free to use by anyone.</p> <br>
      <p></p>"
      ),
      HTML(
      "<p>The project is coordinated by the Department of Rehabilation Sciences at Ghent University.</p>"
      ),
      HTML(
      "<h2><i class='fa fa-vial'></i>&nbsp; Diagnostic Tests</h2>"
      ),
      p(
      "Translating diagnostic accuracy studies into a physio's practice can be a difficult task. By design, many diagnostic studies report mainly the Sensitivity (Sn) and Specificity (Sp) of the evaluated diagnostic tests.
      Unfortunatly both Se and Sp are difficult to interpret in a daily practitc. The basic idea of diagnostic accuracy studies is to
      identify patients with a particular disorder in a pool of individuals. The presence of a particular disease is often noted as
      D+, while its absence is often noted as D-. A similar approach is used for a positive (T+) and negative (T-) test. Se and Sp are
      primarely based on the diagnosis (D) of the patient, and an evaluation of the proportion of tests (T) that are correct and in accordance
      with the accordance with the D. This implies we need to have information on the disease status of the patient, which is only the
      case in a scientific study. However, in practice, we are unaware of the disease status (D+ or D-) of the patient, and only have
      information on the test (T+ or T-). For this purpose, we can use the positive and negative predictive value (PPV and NPV), which
      can be immediatly be translated into a clinical practice as, for example, the PPV can be translated immediatly as the probability
      that your patient actually has the disease. One of the major drawback of the PPV and NPV is that they are depending on the a priori
      probability of having the disease. This is the reason why the PPV and NPV are often useless in the context of a scientific article
      as the a priori probability of being sick is often overestimated (e.g., in diagnostic studies the number of patients often equals
      the number of non-patients). In contrast the Se and Sp is insensible to the a priori probability, which explains their popularity in
      the scientific literature."
      ),
      HTML(
      "<h2><i class='fa fa-user-md'></i>&nbsp; My Patients</h2>"),
      p(
      "This tab focuses on the translation of test combinations in your practice. 
      It allows you to select multiple test that you want to perform in practice,
      and check what the probability is of your patients haveing or not having the 
      disease when all performed tests are negative or at least one test is negative."
      ),
      HTML(
      "<h2><i class='fa fa-table'></i>&nbsp; All data</h2>"
      ),
      HTML(
      "<p>In the <b>All Data</b> tab, you will find all the studies that are included 
      in the current dataset. If you wish to include more studies,
      you can always fill in the following <a href='https://forms.gle/fm9THiUFyhMCjdj69'>survey</a></p>"
      )
  )
}




