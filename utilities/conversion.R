# Methods support working with data in the excel file
Conversion_Utility <- R6Class(
  # Class name
  "Conversion_Utility",
  
  # Public properties and methods
  public = list(
    # 1 pt ≈ 0.35146 mm
    mm_to_pt = function(mm) {
      return(round(mm / 0.35146, digits = 2))
    }
  )
)
