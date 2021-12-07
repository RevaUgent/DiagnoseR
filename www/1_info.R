##-------------- INFO SHEET
InfoUI <- function() {
  tagList(
      h2("BackpocketPhysio project"),
      HTML(
      "<p>The aim of BackpocketPhysio project is to translate science the the daily practice of the physiotherapist/manual therapist.
      There are different apps in development, and the current BackpocketPhysio › Diagnosis APP aims to set-up a datbase that includes 
      information on a wide arnge of diagnostics tests that have been validated in peer-reviewed journals. All includes tests should be applicaple
      in a clinical setting. Based on this information, dataset is complied, and can be freely used by everyone. </p> <br>
      <p>Feedback or requests for functionality are always welcomed and can be sent by email to Robby.DePauw@Ugent.be.</p>"
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
      the scientific literature. In addition, this tab will allow you to get insight into the quality of the different included studies and the ranges of diagnostic outcomes that
      they have been reported."
      ),
      HTML(
      "<h2><i class='fa fa-user-md'></i>&nbsp; My Patients</h2>"),
      p(
      "This tab focuses on the direct translation of clinical questions based on sound scientific research. You can sleect the complaints of your patient,
      and diferent tests will be proposed base don the current scientific evidence. You can select a pre-test probability based on your clinical judgement for
      up to three diagnostic priorities. Based on the result of the test, post-test probabilities will be calculated to support the clinical decision making process."
      ),
      HTML(
      "<h2><i class='fa fa-table'></i>&nbsp; All data</h2>"
      ),
      HTML(
      "<p>In the <b>All Data</b> tab, you will find all the studies that are included 
      in the current dataset. You can also find a download link to the table in diferent formats such as .csv and .xlsx."
      ),
      HTML(
        "<h2><i class='fa fa-hands-helping'></i>&nbsp; Participate</h2>"
      ),
      HTML(
        "<p>In the <b>Participate</b> tab, you can find the link to the survey which allows you to participate to the survey. To do so, 
        please use the following <a href='https://forms.gle/fm9THiUFyhMCjdj69'>survey</a>. We have also included a list of participants we wish to acknowledge for their
        input.</p>"
      )
  )
}




