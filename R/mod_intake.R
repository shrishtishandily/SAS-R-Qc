mod_intake_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "panel",
      shiny::div(class = "section-head", shiny::h2("Reviewer and task")),
      shiny::div(
        class = "form-row",
        shiny::textInput(ns("reviewer"), "Reviewer name (recorded in the audit log)", placeholder = "Your name"),
        shiny::textInput(ns("task"), "Task label", placeholder = "e.g. ADVS independent QC")
      ),
      shiny::p(class = "field-help", "Self-declared name only; the prototype does not authenticate users.")
    ),
    shiny::div(
      class = "panel",
      shiny::div(
        class = "section-head",
        shiny::h2("Upload datasets and programs"),
        shiny::actionButton(ns("demo"), "Load synthetic demo", class = "btn-ghost")
      ),
      shiny::uiOutput(ns("uploads")),
      shiny::uiOutput(ns("loaded"))
    ),
    shiny::uiOutput(ns("exec_panel")),
    shiny::div(
      class = "panel action-panel",
      shiny::uiOutput(ns("confirm_ui"))
    )
  )
}

mod_intake_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    store <- ctx$store

    shiny::observe(store$reviewer <- trimws(input$reviewer %||% ""))
    shiny::observe(store$task <- trimws(input$task %||% ""))

    output$uploads <- shiny::renderUI({
      store$reset_n
      ns <- session$ns
      shiny::div(
        class = "upload-grid",
        shiny::fileInput(ns("ref_file"), "SAS reference dataset (.xpt, .sas7bdat, .csv)", accept = paste0(".", ALLOWED_DATA_EXT)),
        shiny::fileInput(ns("qc_file"), "R QC dataset (.xpt, .sas7bdat, .csv)", accept = paste0(".", ALLOWED_DATA_EXT)),
        shiny::fileInput(ns("sas_file"), "SAS program for review (.sas, .txt)", accept = c(".sas", ".txt")),
        shiny::fileInput(ns("r_file"), "Candidate R QC program (.R, .txt)", accept = c(".r", ".R", ".txt"))
      )
    })

    load_dataset <- function(kind, file) {
      label <- if (kind == "ref") "Reference dataset" else "QC dataset"
      tryCatch({
        df <- read_dataset(file$datapath, file$name)
        meta <- dataset_meta(df, file$datapath, file$name)
        store[[kind]] <- df
        store[[paste0(kind, "_meta")]] <- meta
        ctx$invalidate(sprintf("%s replaced", label))
        ctx$log(sprintf("%s loaded", label), sprintf("file=%s; rows=%d; cols=%d; md5=%s", meta$name, meta$rows, meta$cols, meta$md5))
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
        ctx$log(sprintf("%s rejected", label), conditionMessage(e))
      })
    }

    shiny::observeEvent(input$ref_file, load_dataset("ref", input$ref_file))
    shiny::observeEvent(input$qc_file, load_dataset("qc", input$qc_file))

    load_program <- function(kind, file) {
      tryCatch({
        text <- read_code(file$datapath, file$name)
        store[[paste0(kind, "_code")]] <- text
        store[[paste0(kind, "_code_name")]] <- basename(file$name)
        store$code_review <- NULL
        ctx$log(sprintf("%s program loaded", toupper(kind)), sprintf("file=%s; lines=%d; md5=%s", basename(file$name), length(strsplit(text, "\n")[[1]]), unname(tools::md5sum(file$datapath))))
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
        ctx$log(sprintf("%s program rejected", toupper(kind)), conditionMessage(e))
      })
    }

    shiny::observeEvent(input$sas_file, load_program("sas", input$sas_file))
    shiny::observeEvent(input$r_file, load_program("r", input$r_file))

    shiny::observeEvent(input$demo, {
      d <- demo_datasets()
      store$ref <- normalize_dataset(d$ref)
      store$qc <- normalize_dataset(d$qc)
      store$ref_meta <- list(name = "demo_reference.synthetic", rows = nrow(d$ref), cols = ncol(d$ref), md5 = "n/a (generated)", loaded = now_stamp())
      store$qc_meta <- list(name = "demo_qc.synthetic", rows = nrow(d$qc), cols = ncol(d$qc), md5 = "n/a (generated)", loaded = now_stamp())
      store$sas_code <- d$sas_code
      store$sas_code_name <- "demo_program.sas"
      store$r_code <- d$r_code
      store$r_code_name <- "demo_program.R"
      store$code_review <- NULL
      store$demo_keys <- d$keys
      ctx$invalidate("Synthetic demo loaded")
      ctx$log("Synthetic demo loaded", "Generated in-memory; no files read.")
    })

    dataset_card <- function(title, meta) {
      shiny::div(
        class = "loaded-card",
        shiny::strong(title),
        if (is.null(meta)) {
          shiny::span(class = "muted", "Not loaded")
        } else {
          shiny::tagList(
            shiny::span(meta$name),
            shiny::span(class = "muted", sprintf("%s rows · %d columns", format(meta$rows, big.mark = ","), meta$cols)),
            shiny::span(class = "muted mono", paste("md5", substr(meta$md5, 1, 12)))
          )
        }
      )
    }

    output$loaded <- shiny::renderUI({
      prog <- function(title, name) {
        shiny::div(class = "loaded-card", shiny::strong(title), if (is.null(name)) shiny::span(class = "muted", "Not loaded") else shiny::span(name))
      }
      shiny::div(
        class = "loaded-grid",
        dataset_card("Reference dataset", store$ref_meta),
        dataset_card("QC dataset", store$qc_meta),
        prog("SAS program", store$sas_code_name),
        prog("R program", store$r_code_name)
      )
    })

    output$exec_panel <- shiny::renderUI({
      ns <- session$ns
      if (!r_exec_enabled()) {
        return(shiny::div(
          class = "panel muted-panel",
          shiny::strong("Local R program execution: disabled"),
          shiny::p("Uploaded programs are displayed for review only. To let this app run an uploaded R program on this machine, start R with the environment variable ADAM_QC_ALLOW_R_EXEC=true. SAS programs can never be executed by this app.")
        ))
      }
      shiny::div(
        class = "panel warn-panel",
        shiny::div(class = "section-head", shiny::h2("Run candidate R program locally")),
        shiny::p("The program runs in a separate R process with your own user permissions. This is not a sandbox. Run only programs you trust. The program must write qc_output.csv or qc_output.xpt in its working directory."),
        shiny::fileInput(ns("exec_inputs"), "Input files made available to the program (optional)", multiple = TRUE),
        shiny::checkboxInput(ns("exec_ack"), "I trust this program and accept that it will run with my permissions.", FALSE),
        shiny::actionButton(ns("exec_run"), "Run R program and use output as QC dataset", class = "btn-secondary-wide")
      )
    })

    shiny::observeEvent(input$exec_run, {
      shiny::req(r_exec_enabled())
      if (is.null(store$r_code)) return(shiny::showNotification("Upload an R program first.", type = "warning"))
      if (!isTRUE(input$exec_ack)) return(shiny::showNotification("Acknowledge the execution warning first.", type = "warning"))
      inputs <- if (is.null(input$exec_inputs)) character() else stats::setNames(input$exec_inputs$datapath, input$exec_inputs$name)
      ctx$log("Local R execution started", sprintf("program=%s; input_files=%d", store$r_code_name %||% "", length(inputs)))
      shiny::withProgress(message = "Running R program", value = 0.5, {
        tryCatch({
          res <- run_r_program(store$r_code, inputs)
          store$qc <- res$data
          store$qc_meta <- list(name = "qc_output (from R program)", rows = nrow(res$data), cols = ncol(res$data), md5 = res$md5, loaded = now_stamp())
          ctx$invalidate("QC dataset produced by local R execution")
          ctx$log("Local R execution succeeded", sprintf("rows=%d; cols=%d; md5=%s", nrow(res$data), ncol(res$data), res$md5))
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = 12)
          ctx$log("Local R execution failed", substr(conditionMessage(e), 1, 300))
        })
      })
    })

    output$confirm_ui <- shiny::renderUI({
      ns <- session$ns
      ready <- !is.null(store$ref) && !is.null(store$qc)
      named <- nzchar(store$reviewer)
      if (store$state != "intake") {
        return(shiny::div(class = "ok-note", "Intake confirmed. Change any input above to return to intake."))
      }
      shiny::tagList(
        shiny::actionButton(ns("confirm"), "Confirm intake and continue", class = "btn-primary-wide", disabled = if (!(ready && named)) NA),
        if (!ready) shiny::p(class = "field-help", "Load both a reference and a QC dataset to continue."),
        if (ready && !named) shiny::p(class = "field-help", "Enter a reviewer name to continue.")
      )
    })

    shiny::observeEvent(input$confirm, {
      shiny::req(store$state == "intake", !is.null(store$ref), !is.null(store$qc), nzchar(store$reviewer))
      ctx$transition("configured")
      ctx$log("Intake confirmed", sprintf("task=%s", store$task))
      ctx$goto("code")
    })
  })
}
