#0.3 - working
#0.4 tbd - needs to auto select a pay grade depending on your 2024 grade selection
#0.5 tbd needs to select the correct pay band based on your work schedule
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(shinyjs)

# ========================
# Data
# ========================

pay_scales <- data.frame(
  grade = c("FY1", "FY2", "ST1-2", "CT1-2", "ST3-5", "CT3-4", "ST6-8"),
  base_salary = c(36616, 42008, 49909, 49909, 61825, 61825, 70425)
)

eng_old_pay_scales <- data.frame(
  grade_eng_08 = c("FY1","FY1","FY1","FY2","FY2","FY2","StR","StR","StR","StR","StR","StR","StR","StR","StR","StR"),
  eng_pay_increment = c("0","1","2","0","1","2","0","1","2","3","4","5","6","7","8","9"),
  eng_base_salary_old = c(21862,23226,24591,27116,28889,30663,28976,30749,33226,34723,36529,38336,40143,41948,43755,45562)
)

Banding <- data.frame(
  pay_band_mult_name = c("no band","1C","1B","1A","2B","2A","3"),
  pay_band_mult = c(1,1.2,1.4,1.5,1.5,1.8,2)
)

ltft_r_uk <- data.frame(
  ltft_scale = c("F9","F8","F7","F6","F5","not LTFT"),
  ltft_scale_mult = c(0.9,0.8,0.7,0.6,0.5,1)
)

pay_premia <- data.frame(
  type = c("Academia", "GP", "Psych Core", "Psych HST (3 years)", "Psych HST (4 years)", "Histopathology", 
           "EM/OMFS (3 years)", "EM/OMFS (4 years)", "EM/OMFS (5 years)", 
           "EM/OMFS (6 years)", "EM/OMFS (7 years)", "EM/OMFS (8 years)"),
  f_premia = c(5216, 10690, 4347, 4347, 3260, 5216, 8693, 6520, 5216, 4347, 3726, 3260)
)

# Tax and NI constants (2008)
OLD_PERSONAL_ALLOWANCE <- 6035
OLD_INCOME_BASIC_THRESH <- 40835
OLD_BASIC_BAND <- OLD_INCOME_BASIC_THRESH - OLD_PERSONAL_ALLOWANCE
OLD_BASIC_RATE <- 0.2
OLD_HIGHER_RATE <- 0.4

OLD_NI_THRESHOLD <- 5435
OLD_NI_BASIC_LIMIT <- 40040
OLD_NI_RATE1 <- 0.11
OLD_NI_RATE2 <- 0.01

# Tax and NI constants (2024)
PERSONAL_ALLOWANCE <- 12570
INCOME_BASIC_THRESH <- 50270
INCOME_HIGHER_THRESH <- 125140
TAPER_START <- 100000
TAPER_END <- 125140
BASIC_BAND <- INCOME_BASIC_THRESH - PERSONAL_ALLOWANCE
HIGHER_BAND <- INCOME_HIGHER_THRESH - INCOME_BASIC_THRESH
BASIC_RATE <- 0.2
HIGHER_RATE <- 0.4
ADDITIONAL_RATE <- 0.45

NI_THRESHOLD <- 12570
NI_BASIC_LIMIT <- 50270
NI_RATE1 <- 0.08
NI_RATE2 <- 0.02

# Scottish tax bands (2024/25)
SCOT_STARTER_RATE <- 0.19
SCOT_BASIC_RATE <- 0.20
SCOT_INTERMEDIATE_RATE <- 0.21
SCOT_HIGHER_RATE <- 0.42
SCOT_ADVANCED_RATE <- 0.45
SCOT_TOP_RATE <- 0.48

SCOT_INCOME_THRESH <- 12570
SCOT_STARTER_THRESH <- 15397
SCOT_BASIC_THRESH <- 27491
SCOT_INTERMEDIATE_THRESH <- 43662
SCOT_HIGHER_THRESH <- 75000
SCOT_ADVANCED_THRESH <- 125140

SCOT_STARTER_BAND <- SCOT_STARTER_THRESH - SCOT_INCOME_THRESH
SCOT_BASIC_BAND <- SCOT_BASIC_THRESH - SCOT_STARTER_THRESH
SCOT_INTERMEDIATE_BAND <- SCOT_INTERMEDIATE_THRESH - SCOT_BASIC_THRESH
SCOT_HIGHER_BAND <- SCOT_HIGHER_THRESH - SCOT_INTERMEDIATE_THRESH
SCOT_ADVANCED_BAND <- SCOT_ADVANCED_THRESH - SCOT_HIGHER_THRESH

#INDEX Values
	#April 08
	CPI08 <-84.0
	CPIH08 <-85.6
	RPI08 <- 844.2
	#April 24
	CPI24 <-133.5
	CPIH24 <-132.2
	RPI24 <- 1518.8
	#April 25
	CPI25 <-138.2
	CPIH25 <-137.7
	RPI25 <- 1586.7


# ========================
# UI
# ========================

ui <- bslib::page_fluid(
	useShinyjs(), #scrolling experiment
	
  titlePanel("Resident Doctor Pay comparison for April 2008 vs April 2024"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
# Info
tags$label("This pay comparatator will look at gross pay (and less income tax) for England only (Wales may be added in a future version). It will not consider student loans. Pension contributions may be added in a future version. It will also give you the option for comparing 2008 pay adjusted for CPI or RPI. It also assumes you are under 65 as personal allwoance was different for over 65s in 2008"),

	
#England pay options	
	  h6(strong("Select your grade")),
      radioButtons("grade", label = NULL, choices = pay_scales$grade),
	  
	  tagList(
		h6(strong("LTFT Options")),
		p("If you’re working less than full time (e.g. 80%), tick this box and fill in the fields below to adjust your uplift and weekend frequency correctly. Please be aware that LTFT calculations may be inaccurate",
			class = "text-muted", style = "margin-top: -8px; margin-bottom: 8px; font-size: 0.7em;")
		),
      checkboxInput("is_ltft", "Are you less than full time?", value = FALSE),
      conditionalPanel(
        condition = "input.is_ltft == true",
        numericInput("ltft_percentageA", "LTFT Percentage (%) (100 if full time)", value = 100, min = 0, max = 100, step = 0.5),
		uiOutput("ltft_hint_text")  #Tells the user what the calculator thinks their LTFT percent should be
      ),
	  
      h6(strong("I don't know my work schedule")),
		checkboxInput("know_gross_income", "Calculations without a work schedule may be inaccurate", value = FALSE),
			conditionalPanel(
				condition = "input.know_gross_income == true",
				numericInput("gross_payA", "What's your Gross Income, excluding locums? (£)", value = 36616, min = 0),
				numericInput("non_enhanced_hours", "Average total weekly hours", value = 48, min = 0, max = 56, step = 0.25)
			),

			conditionalPanel(
				condition = "input.know_gross_income == false",
					tagList(
					numericInput("non_enhanced_hours", "Average total weekly hours", value = 44, min = 0, max = 56, step = 0.25),
					sliderInput("enhanced_hours", label = NULL,
								value = 4, min = 0, step = 0.25, max = 56),
					checkboxInput("weekend_work", "Do you work weekends?", value = TRUE),
					conditionalPanel(
					condition = "input.weekend_work == true",
					numericInput("weekend_freq", "If you work weekends, what is your average frequency? (e.g. 1 in 6 = enter 6). This can be a decimal number.", value = 6, min = 1),
    
						# Nested panel: only show if LTFT
						conditionalPanel(
							condition = "input.is_ltft == true",
							numericInput("ft_weekend_freq", "What is the Full-time Weekend Frequency for your rota? (e.g. 1 in 5 = enter 5).", value = 5, min = 1)
						)),
					checkboxInput("is_on_call_allowance", "Do you get an on-call allowance?", value = FALSE),
						conditionalPanel(
						condition = "input.is_ltft == true && input.is_on_call_allowance == true",
						numericInput("NROC_percent", "What percent of the non-resident on call rota do you do compared to your full time colleagues? (%)", value = 100, min = 0, max = 100, step = 0.1),
						uiOutput("NROC_hint_text")  #Tells the user what the calculator thinks their LTFT percent should be
						)
					)
			),
           
      uiOutput("premia_selector"),
      
      h6(strong("London Weighting")),
      checkboxInput("is_London_weight", "Do you get London Weighting?", value = FALSE),
      conditionalPanel(
        condition = "input.is_London_weight == true",
        selectInput("london_weighting", "London Weighting Band:",
                    choices = c("Fringe", "Outer", "Inner"),
                    selected = "Inner")
      ),
	  
 h6(strong("Which measure of inflation do you want to use?")),
      h6(strong("Select Inflation measure")),
					        radioButtons(
							"inflation_type", "please select",
							choices = c("CPI","CPIH","RPI"),
							selected = "RPI",
							inline = TRUE
							),	

	  
 h6(strong("Options for 2008 pay")),
      checkboxInput("is_advanced", "Advanced options (incl LTFT)", value = FALSE),	  
	  #Advanced options	  
	  conditionalPanel(
		condition = "input.is_advanced == true",
      h6(strong("Select Grade")),
					        radioButtons(
							"grades_eng_08", "What's your grade?",
							choices = c("FY1/FHO1", "FY2/FHO2", "SpR/StR"),
							selected = "SpR/StR",
							inline = TRUE
							),	
					
					      h6(strong("Select Incremental Pay")),
					        radioButtons(
							"pay_eng_08", "Within your grade, what incremental point are you at?",
							choices = c("0/min", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
							selected = "0/min",
							inline = TRUE
							)
					),	
	  			
#LTFT + banding 				
	  conditionalPanel(
		condition = "input.is_advanced == true", 
		tagList(
			h6(strong("LTFT Options")),
			p("If you’re working less than full time, tick the box and choose your LTFT band",
				class = "text-muted", style = "margin-top: -8px; margin-bottom: 8px; font-size: 0.9em;")
		),
      checkboxInput("is_ltft", "Are you less than full time?", value = FALSE),
		conditionalPanel(
			condition = "input.is_ltft == true",
			radioButtons(
				"LTFT_old", "What would your LTFT scale have been in 2008 (F9 is ~90%, F8 is ~80% etc.)?",
				choices = c("F9", "F8", "F7", "F6", "F5", "not LTFT"),
				selected = "F8",
				inline = TRUE
			),
		numericInput("ltft_hoursA", "What are your average weekly hours? (use 40 if Full time)", value = 40, min = 0, max = 56)
		),
	  
     h6(strong("Banding Options")),
		conditionalPanel(
			condition = "!( (input.is_advanced == true) && (input.grades_old == 'FY1/FHO1' || input.grades_old == 'FY2/FHO2') )",
			checkboxInput("gp_band", "If you are a GPST, are you currently on a GP placement? (If, in 2008, you would have gotten GP banding regardless of placement tick this box)", value = FALSE)
		),
			conditionalPanel(
				condition = "input.gp_band == false",
				checkboxInput("know_band", "Do you know your pay band? If not we'll assume no banding", value = TRUE),
					conditionalPanel(
						condition = "input.know_band == true",
					       radioButtons(
								"pay_band", "What's your band?",
								choices = c("no band", "1C", "1B", "1A", "2B", "2A", "3","FC","FB","FA","GP"),
								selected = "no band",
								inline = TRUE
							)				
					)
			)
		),
	  
#Common options	  
	  h6(strong("Tax Region")),
      checkboxInput("is_scotland", "Are you a Scottish taxpayer?", value = FALSE),
	  
      actionButton("calculate", "Calculate Uplift")
    ),
    mainPanel(
		div(id = "results_section",  # ← This is the scroll target
			uiOutput("salary_output"),
			plotOutput("barplot_output"),
			uiOutput("feedback_message")
			
		)
    )
)
)

server <- function(input, output, session) {

# ========================
# Server
# ========================
    
  vals <- reactiveValues(salary_html = NULL) # prevent output changes if someone changes input before pressing calc
  
	output$ltft_hint_text <- renderUI({
			if (!input$is_ltft || is.null(input$non_enhanced_hours) || is.null(input$enhanced_hours) ||
				is.na(input$non_enhanced_hours) || is.na(input$enhanced_hours)  || input$know_gross_income) {
			return(NULL)
		}

		basic_hours <- input$non_enhanced_hours - input$enhanced_hours

		if (basic_hours < 0) {
			return(tags$div(style = "color: red;", "Enhanced hours cannot exceed total hours."))
		}

		calculated_pct <- round((basic_hours / 40) * 100, 1)
		tags$div(
			style = "color: #0d6efd; font-size: 90%; margin-top: -10px;",
			HTML(paste0("Hint: Your LTFT % should be approximately ", calculated_pct, 
            "% based on your hours (", basic_hours, " basic hrs/wk).<br>",
            "If you use a percentage that's too different, the result may be inaccurate."))
		)
	})

	output$NROC_hint_text <- renderUI({
			if (!input$is_ltft || !input$is_on_call_allowance || is.na(input$is_on_call_allowance)) {
			return(NULL)
		}

		if ((input$is_ltft && input$is_on_call_allowance) && is.null(input$NROC_percent)) {
			return(tags$div(style = "color: red;", "Please input the percentage of the non-resident on-call rota you do compared to your full time colleagues."))
		}
		tags$div(
			style = "color: #0d6efd; font-size: 90%; margin-top: -10px;",
			paste0("If you are LTFT this is likely less than 100%, but if you work the full NROC rota despite being LTFT please use 100%.")
		)
	})

	observe({
		total_hours <- input$non_enhanced_hours
		current_enhanced <- input$enhanced_hours
		if (!is.null(total_hours) && !is.na(total_hours) && total_hours >= 0) {
			new_enhanced <- min(current_enhanced, total_hours)
			updateSliderInput(
				session, 
				"enhanced_hours", 
				max = total_hours, 
				value = new_enhanced,
				label = paste0("How many of those ", total_hours, " hours are enhanced hours (hours that attract a 37% enhancment)?"))
		}
	})
 
# ========================
# Pay premia logic
# ========================
 
  output$premia_selector <- renderUI({
    grade <- input$grade
    choices <- if (grade %in% c("FY1", "FY2")) {
		c("Academia")  # only academia allowed
	} else {
		pay_premia$type
	}
    
    tagList(
		div(
			style = "display: flex; align-items: center; gap: 8px;",
			h6(strong("Flexible Pay Premia Type")),
			tags$span(
				bs_icon("info-circle", size = "1.25em", class = "text-primary"),
				`data-bs-toggle` = "popover",
				`data-bs-trigger` = "focus",  # ensures it works on mobile (on click/tap)
				`data-bs-placement` = "right",
				`data-bs-content` = "Generally you should only select up to two options. You can select more, but there don't seem to be any realistic combinations other than Academia + one other.",
				tabindex = "0",  # makes it focusable so popover can be triggered
				style = "cursor: pointer;"
			)
		),
	checkboxGroupInput(
      "flexible_pay_premia",
      label = NULL,
      choices = choices,
      selected = NULL
    ),
    # Add an empty div to inject the JS script after UI loads
    tags$script(HTML("
      // Initialize all popovers on the page
      var popoverTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"popover\"]'))
      var popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
        return new bootstrap.Popover(popoverTriggerEl)
      });
    ")
	)
  )
})
  
# ========================
# Select increment manually if advanced is selected
# ========================

	observe({
		if (input$is_advanced) {# Determine allowed increments based on grade
			max_increment <- switch(input$grades_eng_08,
                            "FY1/FHO1" = 2,
                            "FY2/FHO2" = 2,
                            "SpR/StR" = 9,
                            2)  # Default fallback
    
			choices <- as.character(0:max_increment)
			labels <- ifelse(choices == "0", "0/min", choices)
				# Update radio buttons choices and select default "0"
			updateRadioButtons(session, "pay_eng_08",
                       choices = setNames(choices, labels),
                       selected = choices[1])
		}
	})
	#GP band reset if FY1/FY2
	observe({
		if (
			(input$grades_eng_08 %in% c("FY1/FHO1", "FY2/FHO2"))
		) {
			updateCheckboxInput(session, "gp_band", value = FALSE)
		}
	})

#GP band overrides LTFT banding, i think (but unclear how it interacts with the F5-F9 stuff), if GP box is ticked, this won't even appear
	observeEvent(input$is_ltft, {
		if(input$is_advanced && input$is_ltft) {
			bands <- c("no band", "FC", "FB", "FA")
		} else {
			bands <- c("no band", "1C", "1B", "1A", "2B", "2A", "3")
		}
  
		updateRadioButtons(
			session,
			"pay_band",
			choices = bands,
			selected = "no band"
		)
	})
 
    observeEvent(input$calculate, {

   #Common inputs/ things that break the app if not zero'd
    #ltft
	ltft_premia <- if (input$is_ltft) 1000 else 0
	ltft_percentage <- if (input$is_ltft && !is.null(input$ltft_percentageA)) input$ltft_percentageA else 100   
    ltft_multiplier <- ltft_percentage / 100
	selected_weight <- 0
	adj_weight <- 0
	band_mult <- 1
	ltft_mult <- 1
	total_time <-0
	enhanced_time <-0
	basic_hours <-0



	
	#inputs	
    get_london_weighting <- function(band) {
      switch(band,
             "Fringe" = 149,
             "Outer"  = 527,
             "Inner"  = 2162,
             0)
    }
  #2024 pay  
	#select correct base pay
    base_salary <- pay_scales %>% filter(grade == input$grade) %>% pull(base_salary)
    #handle null for pay premia
	selected_premia <- if (is.null(input$flexible_pay_premia)) {
		0
	} else {
		sum(pay_premia$f_premia[pay_premia$type %in% input$flexible_pay_premia], na.rm = TRUE)
	}
	#london weight
    selected_weight <- if (input$is_London_weight) get_london_weighting(input$london_weighting) else 0

    #adjustments			 
    adj_grade <- base_salary * ltft_multiplier
    adj_premia <- selected_premia * ltft_multiplier
    adj_weight <- selected_weight * ltft_multiplier
	
	# Initialise values
	enhanced_pay <- 0
	non_enhanced_pay <- adj_grade
	adj_weekend_pay <- 0
	adj_on_call <- 0

	# Input validation
	if (is.null(input$non_enhanced_hours) || is.null(input$enhanced_hours) ||
		is.na(input$non_enhanced_hours) || is.na(input$enhanced_hours)) {
	showNotification("Please enter your average total weekly hours and enhanced hours.", type = "error")
	return()
	}
	total_time <- input$non_enhanced_hours	
	enhanced_time <- input$enhanced_hours
	basic_hours <- total_time - enhanced_time

	if (input$know_gross_income) {
		if (is.null(input$gross_payA) || is.na(input$gross_payA)) {
		showNotification("Please enter your gross pay.", type = "error")
		return()
	}
	gross_pay <- input$gross_payA
	} else {

	# Estimate based on rota
	base_hourly <- base_salary / 40 / 52
	enhanced_multiplier <- 1.37
	non_enhanced_multiplier <- 1.00

	# Weekend frequency validation
	if (is.null(input$weekend_freq) || is.na(input$weekend_freq) || input$weekend_freq < 1) {
		showNotification("Please enter a valid Weekend Frequency (e.g. 5 for 1 in 5).", type = "error")
		return()
	}

	if (input$is_ltft) {
		if (is.null(input$ft_weekend_freq) || is.na(input$ft_weekend_freq) || input$ft_weekend_freq < 1) {
		showNotification("Please enter a valid full-time Weekend Frequency.", type = "error")
		return()
		}
	}

	weekend_freq <- input$weekend_freq
	ft_weekend_freq <- if (input$is_ltft) input$ft_weekend_freq else input$weekend_freq
	if (ft_weekend_freq > weekend_freq){
		ft_weekend_freq <- weekend_freq
	}
	
	if(!input$weekend_work){
		weekend_freq <- 9
		ft_weekend_freq <- 9
	}
		

	weekend_freq_percent <- ft_weekend_freq / weekend_freq

	weekend_supplement <- function(freq, salary) {
		if (freq <= 2) return(salary * 0.15)
		if (freq <= 3) return(salary * 0.10)
		if (freq <= 4) return(salary * 0.075)
		if (freq <= 5) return(salary * 0.06)
		if (freq <= 6) return(salary * 0.05)
		if (freq <= 7) return(salary * 0.04)
		if (freq <= 8) return(salary * 0.03)
		return(0)
	}

	weekend_pay <- weekend_supplement(ft_weekend_freq, base_salary)
	adj_weekend_pay <- weekend_pay * weekend_freq_percent

	# On-call
	if(!input$is_ltft){
		NROC_percent <-1
	}	else {NROC_percent <- input$NROC_percent/100
	}
	on_call_allowance <- if (input$is_on_call_allowance) base_salary * 0.08 else 0
	adj_on_call <- NROC_percent * on_call_allowance

	# OOH Pay breakdown
	enhanced_pa <- input$enhanced_hours * base_hourly * enhanced_multiplier * 52
	non_enhanced_pa <- basic_hours * base_hourly * non_enhanced_multiplier * 52
	enhanced_pay <- enhanced_pa * (0.37/1.37)
	non_enhanced_pay <- non_enhanced_pa + enhanced_pa * (1/1.37)

	# Gross
	gross_pay <- non_enhanced_pay + enhanced_pay + adj_weekend_pay + adj_on_call + adj_premia + adj_weight + ltft_premia
	Gross_less_OOH <- adj_premia + adj_weight + ltft_premia + adj_grade
	}

		#Gross less OOH, and new_gross:
			if (input$know_gross_income) {
			OOH_pay <- gross_pay - (adj_grade + adj_premia + adj_weight + ltft_premia)
			Gross_less_OOH <- gross_pay - OOH_pay
			    
			} else {OOH_pay <- non_enhanced_pay + enhanced_pay + adj_weekend_pay + adj_on_call - adj_grade #if user has been accurate, adj_grade should equal non_enhanced_pay
					Gross_less_OOH <- gross_pay - OOH_pay
					
			} 
#end 2024 pay			
	#Warn about human error
	#Should Only do the below if nation == england
	
			#If Ltft percentage does not match basic hours
			expected_basic_hours <- ltft_multiplier * 40
			tolerance <- 2  # hours

			if (input$is_ltft && !input$know_gross_income && abs(basic_hours - expected_basic_hours) > tolerance) {
				showNotification("Warning: Your LTFT percentage and basic hours may not match. Please double-check your inputs. Results may be wrong.", type = "warning")
			}
			
			#less than 40hrs, but not LTFT
			if (!input$is_ltft && input$non_enhanced_hours < 40) {
					showNotification("Warning: You do less than 40 hours per week, but you did not tick LTFT. Is this correct?", type = "warning")
				}
			
				
			#input gross pay is less than (estimated minimum gross, using inputs)
			if (input$know_gross_income && gross_pay < Gross_less_OOH) {
					showNotification("Error: The total pay you entered was below the minimum expected - based on your London weighting, LTFT allowance, pay premia and base pay. Please enter the correct gross income", type = "error")
					return() #this line stops the calculations from proceeding
				}
			
			#input hours are over 48 (with null input check)
			if (!is.null(input$non_enhanced_hours) && !is.na(input$non_enhanced_hours) &&
					input$non_enhanced_hours > 48) {
					showNotification("Warning: The total of the enhanced + non enhanced hours you entered are greater than 48. If you have not opted out of the EWTD your hours should be 48 or less. If you have opted out, your hours should be 56 or less. Are your hours correct?", type = "warning")
				}
			
			#if ltft weekend frequency is higher than ft weekend frequency
			if (input$is_ltft && input$ft_weekend_freq > input$weekend_freq){
				showNotification("Warning: You are LTFT and have stated you do more frequent weekends than your full time colleagues. Unfortunately I do not know the correct way to handle this info, so I am assuming I do not pro-rata this.", type = "warning")
			}

	#2008 Pay
			eng_base_salary_2008 <- 0
			# Map radio button to data frame format
			grade_map <- list(
				"FY1/FHO1" = "FY1",
				"FY2/FHO2" = "FY2",
				"SpR/StR" = "StR"
			)
  
			selected_grade <- grade_map[[input$grades_eng_08]]
			if (is.null(selected_grade)) {
				selected_grade <- "FY1"  # fallback or handle error
			}
			selected_increment <- if (!is.null(input$pay_eng_08)) input$pay_eng_08 else "0"  # Already values like "0", "1", ..., "9"

			# Filter the relevant row
			matched_row <- subset(eng_old_pay_scales,
									grade_eng_08 == selected_grade & eng_pay_increment == selected_increment)

			# You can choose between old or new depending on your calculator version
			if (nrow(matched_row) == 1) {
				eng_base_salary_2008 <- matched_row$eng_base_salary_old
			} else {
				eng_base_salary_2008 <- 0  # or throw a warning
			}
		
		
		#base salary stuff 08		
		base_salary_08 <- eng_base_salary_2008
		gross_pay_08 <- eng_base_salary_2008
		

			
	#get pay band mult
	band_mult <-1
		 		
		# Map radio button to data frame format
			grade_map <- list(
				"FY1/FHO1" = "FY1",
				"FY2/FHO2" = "FY2",
				"SpR/StR" = "StR"
			)
  
			selected_grade <- grade_map[[input$grades_eng_08]]
			 
		 
			selected_band <- if (!is.null(input$pay_band)) input$pay_band else "no band"  # no_band, 1c,1b etc.

			# Filter the relevant row
			matched_band_row <- subset(Banding,
									pay_band_mult_name == selected_band)

			if(input$gp_band) {
				band_mult <- 1.5 #gp_band override
			} else if (selected_grade == "FY1" && selected_band == "no band") {
				band_mult <- 1
			} else if (nrow(matched_band_row) == 1) {
				band_mult <- matched_band_row$pay_band_mult
			} else {
				band_mult <- 1  # or throw a warning
			}
#in 2009 GP band changed to 1.45
#in 2010 F1s got 5% banding ate baseline		
	# get ltft mult	
	
	ltft_mult_08 <- 1
		if (input$is_ltft) {
		 
			selected_ltft <- if (!is.null(input$LTFT_old)) input$LTFT_old else "F8"  # f9,f8...

			# Filter the relevant row
			matched_ltft_row <- subset(ltft_r_uk,
									ltft_scale == selected_ltft)
			
			if (nrow(matched_ltft_row) == 1) {
				ltft_mult_08 <- matched_ltft_row$ltft_scale_mult
			} else {
				ltft_mult_08 <- 1  # or throw a warning
			}
		}
		
gross_pay_08 <- ceiling(ceiling(base_salary_08 * ltft_mult_08) * band_mult) + selected_weight


unband <- ceiling(base_salary_08 * ltft_mult_08) + selected_weight


band_enhance <- gross_pay_08 - unband

band_percent = (band_mult-1) * 100
		

	
    #Income Tax and NI						   
    #Income Tax
    income_tax <- function(gross, is_scotland = FALSE) {
      pa <- PERSONAL_ALLOWANCE
      if (gross > TAPER_START && gross <= TAPER_END) {
        pa <- max(0, PERSONAL_ALLOWANCE - (gross - TAPER_START) / 2)
      } else if (gross > TAPER_END) {
        pa <- 0
      }
	  
	  #account for marginal rate
	  bandwidth_increase <- PERSONAL_ALLOWANCE - pa
	  HIGHER_BAND <- HIGHER_BAND + bandwidth_increase
	  SCOT_ADVANCED_BAND <- SCOT_ADVANCED_BAND + bandwidth_increase
      
      taxable_income <- max(0, gross - pa)
      
      if (!is_scotland) {
        # England/Wales/NI tax bands
        if (taxable_income <= BASIC_BAND) {
          return(taxable_income * BASIC_RATE)
        } else if (taxable_income <= BASIC_BAND + HIGHER_BAND) {
          return(BASIC_BAND * BASIC_RATE +
                   (taxable_income - BASIC_BAND) * HIGHER_RATE)
        } else {
          return(BASIC_BAND * BASIC_RATE +
                   HIGHER_BAND * HIGHER_RATE +
                   (taxable_income - BASIC_BAND - HIGHER_BAND) * ADDITIONAL_RATE)
        }
      }

	  else {
    # Scotland tax bands
    if (taxable_income <= SCOT_STARTER_BAND) {
      return(taxable_income * SCOT_STARTER_RATE)
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        (taxable_income - SCOT_STARTER_BAND) * SCOT_BASIC_RATE
      )
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND + SCOT_INTERMEDIATE_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND) * SCOT_INTERMEDIATE_RATE
      )
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND + SCOT_INTERMEDIATE_BAND + SCOT_HIGHER_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        SCOT_INTERMEDIATE_BAND * SCOT_INTERMEDIATE_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND - SCOT_INTERMEDIATE_BAND) * SCOT_HIGHER_RATE
      )
    } else if (taxable_income <= SCOT_STARTER_BAND + SCOT_BASIC_BAND + SCOT_INTERMEDIATE_BAND + SCOT_HIGHER_BAND + SCOT_ADVANCED_BAND) {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        SCOT_INTERMEDIATE_BAND * SCOT_INTERMEDIATE_RATE +
        SCOT_HIGHER_BAND * SCOT_HIGHER_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND - SCOT_INTERMEDIATE_BAND - SCOT_HIGHER_BAND) * SCOT_ADVANCED_RATE
      )
    } else {
      return(
        SCOT_STARTER_BAND * SCOT_STARTER_RATE +
        SCOT_BASIC_BAND * SCOT_BASIC_RATE +
        SCOT_INTERMEDIATE_BAND * SCOT_INTERMEDIATE_RATE +
        SCOT_HIGHER_BAND * SCOT_HIGHER_RATE +
        SCOT_ADVANCED_BAND * SCOT_ADVANCED_RATE +
        (taxable_income - SCOT_STARTER_BAND - SCOT_BASIC_BAND - SCOT_INTERMEDIATE_BAND - SCOT_HIGHER_BAND - SCOT_ADVANCED_BAND) * SCOT_TOP_RATE
      )
    }
  }
}


income_tax_2008 <- function(gross) {
		taxable_income <- max(0, gross - OLD_PERSONAL_ALLOWANCE)

		if (taxable_income <= OLD_BASIC_BAND) {
          return(taxable_income * OLD_BASIC_RATE)
        } else if (taxable_income >= OLD_BASIC_BAND) {
          return(OLD_BASIC_BAND * OLD_BASIC_RATE +
                   (taxable_income - OLD_BASIC_BAND) * OLD_HIGHER_RATE)
        } else {
          return(0)
      }
    }

    #NI
    ni_calc <- function(gross) {
      if (gross <= NI_THRESHOLD) {
        return(0)
      } else if (gross <= NI_BASIC_LIMIT) {
        return((gross - NI_THRESHOLD) * NI_RATE1)
      } else {
        basic_ni_band <- NI_BASIC_LIMIT - NI_THRESHOLD
        upper_ni_band <- gross - NI_BASIC_LIMIT
        return(basic_ni_band * NI_RATE1 + upper_ni_band * NI_RATE2)
      }
    }
	    ni_calc_08 <- function(gross) {
      if (gross <= OLD_NI_THRESHOLD) {
        return(0)
      } else if (gross <= OLD_NI_BASIC_LIMIT) {
        return((gross - OLD_NI_THRESHOLD) * OLD_NI_RATE1)
      } else {
        basic_ni_band <- OLD_NI_BASIC_LIMIT - OLD_NI_THRESHOLD
        upper_ni_band <- gross - OLD_NI_BASIC_LIMIT
        return(basic_ni_band * OLD_NI_RATE1 + upper_ni_band * OLD_NI_RATE2)
      }
    }
    
    scotland_flag <- isTRUE(input$is_scotland)
    income_tax_24 <- income_tax(gross_pay, scotland_flag)
    income_tax_08 <- income_tax_2008(gross_pay_08)
    
    ni_24 <- ni_calc(gross_pay)
    ni_08 <- ni_calc_08(gross_pay_08)
    
    #Other outputs				
    net_24 <- gross_pay - income_tax_24 - ni_24
    net_08 <- gross_pay_08 - income_tax_08 - ni_08
	
	#inflation adjust outputs
	CPI08to24 <- CPI24/CPI08
	CPIH08to24 <- CPIH24/CPIH08
	RPI08to24 <- RPI24/RPI08
    
	inflation_type <- input$inflation_type
	if(inflation_type == "RPI"){infl<- RPI08to24
	} else if (inflation_type == "CPI"){infl<- CPI08to24
	} else if (inflation_type == "CPIH"){infl<- CPIH08to24
	}
	
	base_salary_08_infl <- base_salary_08 * infl
	net_08_infl <- net_08 * infl
	gross_pay_08_infl <- gross_pay_08 * infl
	
  
# ========================
# HTML outputs
# ========================

  # Helper to safely format money
  fmt <- function(x) sprintf("£%s", formatC(x, format = "f", digits = 2, big.mark = ","))

  # Only HTML text strings allowed — this builds the final HTML
  out <- c()

  # 2024 Pay Section
  out <- c(out, "<h4>2024 Pay</h4>")
  out <- c(out, sprintf("<p><strong>Gross Pay:</strong> %s</p>", fmt(gross_pay)))

  # Breakdown (England detailed)
  if (!input$know_gross_income) {
    out <- c(out, "<ul>")
    out <- c(out, sprintf("<li>Regular Pay (%s hrs): %s</li>", total_time, fmt(non_enhanced_pay)))
    if (enhanced_pay > 0) out <- c(out, sprintf("<li>Enhanced Pay (%s hrs @ 37%%): %s</li>", enhanced_time, fmt(enhanced_pay)))
    if (adj_weekend_pay > 0) out <- c(out, sprintf("<li>Weekend Allowance: %s</li>", fmt(adj_weekend_pay)))
    if (adj_on_call > 0) out <- c(out, sprintf("<li>On-call Availability: %s</li>", fmt(adj_on_call)))
	out <- c(out, sprintf("<li>Base Pay for your grade (40 hrs): %s</li>", fmt(base_salary)))
	out <- c(out, "</ul>")
	
    } else if (input$know_gross_income) {
    out <- c(out, "<ul>")
    out <- c(out, sprintf("<li>Base Pay: %s</li>", fmt(adj_grade)))
    if (OOH_pay > 0)out <- c(out, sprintf("<li>OOH Pay: %s</li>", fmt(OOH_pay)))
    out <- c(out, "</ul>")
  }

  # Add universal items for England 
		if (adj_premia > 0) out <- c(out, sprintf("<p>Pay Premia: %s</p>", fmt(adj_premia)))
		if (adj_weight > 0) out <- c(out, sprintf("<p>London Weighting: %s</p>", fmt(adj_weight)))
		if (ltft_premia > 0) out <- c(out, sprintf("<p>LTFT Allowance: %s</p>", fmt(ltft_premia)))	
	
  # Deductions (universal items)
  out <- c(out, "<p><strong>Deductions:</strong></p><ul>")
  out <- c(out, sprintf("<li>Income Tax: %s</li>", fmt(income_tax_24)))
  out <- c(out, sprintf("<li>National Insurance: %s</li>", fmt(ni_24)))
  out <- c(out, "</ul>")

  out <- c(out, sprintf("<p><strong>Net Pay:</strong> %s</p>", fmt(net_24)))


  # 2008 Pay
  out <- c(out, "<hr><h4>2008 Pay (not adjusted)</h4>")
  out <- c(out, sprintf("<p><strong>Gross Pay:</strong> %s</p>", fmt(gross_pay_08)))

  if (band_mult > 1) {
    out <- c(out, "<ul>")
    if (unband > 0) out <- c(out, sprintf("<li>Base Pay: %s</li>", fmt(unband)))
    out <- c(out, sprintf("<li>Banding Uplift (%s%%): %s</li>", band_percent, fmt(band_enhance)))
    out <- c(out, "</ul>")
  }
 
 # Add universal items for England 
		if (selected_weight > 0) out <- c(out, sprintf("<p>London Weighting: %s</p>", fmt(selected_weight)))


  out <- c(out, "<p><strong>Deductions:</strong></p><ul>")

  out <- c(out, sprintf("<li>Income Tax: %s</li>", fmt(income_tax_08)))
  out <- c(out, sprintf("<li>National Insurance: %s</li>", fmt(ni_08)))
  out <- c(out, "</ul>")

  out <- c(out, sprintf("<p><strong>Net Pay:</strong> %s</p>", fmt(net_08)))


  # Inflation summary
  out <- c(out, "<hr><h4>Inflation adjusted figures</h4><ul>")
  out <- c(out, sprintf("<li>2008 Gross pay adjusted for inflation: %s</li>", fmt(gross_pay_08_infl)))
  out <- c(out, sprintf("<li>2008 Net pay adjusted for inflation: %s</li>", fmt(net_08_infl)))
  out <- c(out, sprintf("<li>2008 Base pay adjusted for inflation: %s</li>", fmt(base_salary_08_infl)))
  out <- c(out, "</ul>")
  
    # Inflation summary percent
  out <- c(out, "<hr><h4>Percentage difference in inflation adjusted figures</h4><ul>")
  out <- c(out, sprintf("<li>Difference in Gross pay %.2f%%</li>", (gross_pay_08_infl/gross_pay -1)))
  out <- c(out, sprintf("<li>Difference in Net pay %.2f%%</li>", (net_08_infl/net_24 -1)))
  out <- c(out, sprintf("<li>Difference in Base pay %.2f%%</li>", (base_salary_08_infl/base_salary -1)))
  out <- c(out, "</ul>")

	#stroing values for the barplots
	vals$net_24 <- net_24
	vals$net_08 <- net_08
	vals$gross_pay <- gross_pay
	vals$gross_08 <- gross_pay_08
	vals$income_tax_08 <- income_tax_08
	vals$income_tax_24 <- income_tax_24
	vals$ni_08 <- ni_08
	vals$ni_24 <- ni_24
	vals$gross_infl <- gross_pay_08_infl
	vals$net_infl <- net_08_infl

  vals$salary_html <- HTML(paste(out, collapse = "\n"))
  })  # <-- This closes observeEvent

 output$salary_output <- renderUI({
    vals$salary_html
  })    # <-- This closes renderUI
	

	
	#barplot render
#		  output$barplot_output <- renderPlot({
#
#	req(input$calculate)  # This should make it render only after pressing Calculate
#		  
#    net_values <- c(vals$net_08, vals$net_24)
#		deduction_values <- c(
#			vals$income_tax_08 + vals$ni_08,
#			vals$income_tax_24 + vals$ni_24
#		)
#
#    values_matrix <- rbind(Net = net_values, Deductions = deduction_values)
#
#    
#	par(mgp = c(4, 0.5, 0))  # Moves the y-axis label further from tick labels
#	bp <- barplot(values_matrix,
#            col = c("steelblue", "tomato"),
#            names.arg = c("Pre-uplift", "Post-uplift"),
#            ylim = c(0, max(colSums(values_matrix)) * 1.1),
#            main = "Net Pay vs Deductions (Old vs New)",
#            ylab = "(£)",
#            legend.text = TRUE,
#            args.legend = list(x = "topright"),
#			yaxt = "n")
#			
	# Add custom y-axis with comma formatting
#			y_ticks <- pretty(c(0, max(colSums(values_matrix)) * 1.1))
#			axis(2, at = y_ticks, labels = format(y_ticks, big.mark = ","), las = 1)
#  
#		# Add text labels on top of each bar segment
#			cumul_values <- apply(values_matrix, 2, cumsum)
#			for (i in 1:ncol(values_matrix)) {
#				for (j in 1:nrow(values_matrix)) {
#					height <- cumul_values[j, i]
#					label <- format(values_matrix[j, i], big.mark = ",")
#					text(x = bp[i], y = height - values_matrix[j, i]/2, labels = label, col = "white", cex = 0.9)
#				}
#			} # closes text value on bar segments
#		}) # closes barplot render

	

  
  #scrolling
  observeEvent(input$calculate, {
    runjs("
      const results = document.getElementById('results_section');
      if (results) {
        const isMobile = /iPhone|iPad|Android/i.test(navigator.userAgent);
        const yOffset = isMobile ? -50 : -150;
        const y = results.getBoundingClientRect().top + window.pageYOffset + yOffset;
        window.scrollTo({top: y, behavior: 'smooth'});

        results.classList.remove('flash');
        void results.offsetWidth;
        results.classList.add('flash');
      }
    ")  
  })
  
	output$feedback_message <- renderUI({
		#req(input$calculate)  # This should make it render only after pressing Calculate
		tags$p(
			style = "font-size: 0.9em; color: #6c757d; margin-top: 20px;",
			"This is a work in progress. If you have found an error, please let me know at ",
			tags$a(href = "https://github.com/viren-bajaj/DDRB-uplift-calc-2025-2026/issues", target = "_blank", "my GitHub page.")
		)
	})
  
}  # <-- This closes server

shinyApp(ui, server)

