
// ------------------------------------------------------------------

var techela_add_check_handler = function (event)
    {Jupyter.notebook.insert_cell_below();
     Jupyter.notebook.select_next();
     Jupyter.notebook.to_markdown();
     Jupyter.notebook.edit_mode();

     var kernel = Jupyter.notebook.kernel;
     kernel.execute("import getpass; username=getpass.getuser(); print(username)",
		    {iopub: {output: function(response) {
			var resp = response.content.text;
			console.log(response.content);
			var text = '<font color="red"> ' + resp + ': ✓</font>';
			var cell = Jupyter.notebook.get_selected_cell();
			cell.set_text(text);
			cell.metadata.type = "comment";
			cell.metadata.content = "Check";
			Jupyter.notebook.execute_cell();}}},
		    {silent: false,
		     store_history: false,
		     stop_on_error: true})}

var techela_add_check_action = {
    help : 'Add comment cell with a checkmark in it.',
    help_index : 'zz',
    handler : techela_add_check_handler }

var add_check = Jupyter.actions.register(techela_add_check_action, 'Add check', 'techela');

Jupyter.keyboard_manager.command_shortcuts.add_shortcut('k', add_check);

// ------------------------------------------------------------------

function techela_add_comment (event)
    {Jupyter.notebook.insert_cell_below();
     Jupyter.notebook.select_next();
     Jupyter.notebook.to_markdown();
     Jupyter.notebook.edit_mode();

     var kernel = Jupyter.notebook.kernel;
     kernel.execute("import getpass; username=getpass.getuser(); print(username)",
		    {iopub: {output: function(response) {
			var resp = response.content.text;
			console.log(response.content);
			var comment = prompt("Comment: ");
			var text = '<font color="red">' + resp + ": "+ comment + '</font>';
			var cell = Jupyter.notebook.get_selected_cell();
			cell.set_text(text);
			cell.metadata.type = "comment";
			cell.metadata.content = comment;
			Jupyter.notebook.execute_cell();
			Jupyter.notebook.command_mode();}}},
		    {silent: false,
		     store_history: false,
		     stop_on_error: true});}

var add_comment = Jupyter.actions.register(techela_add_comment, 'Add comment', 'techela');

Jupyter.keyboard_manager.command_shortcuts.add_shortcut('c', add_comment);

// ------------------------------------------------------------------

function techela_add_grade (event)
    {function letter_to_number(letter)
     {var n = "none";
      if (letter == 'A++')  {n=1.0}
      else if (letter == 'A+')  {n=0.95}
      else if (letter == 'A')  {n=0.9}
      else if (letter == 'A-')  {n=0.85}
      else if (letter == 'A/B')  {n=0.8}
      else if (letter == 'B+')  {n=0.75}
      else if (letter == 'B')  {n=0.7}
      else if (letter == 'B-')  {n=0.65}
      else if (letter == 'B/C')  {n=0.6}
      else if (letter == 'C+')  {n=0.55}
      else if (letter == 'C')  {n=0.5}
      else if (letter == 'C-')  {n=0.45}
      else if (letter == 'C/D')  {n=0.4}
      else if (letter == 'D+')  {n=0.35}
      else if (letter == 'D')  {n=0.3}
      else if (letter == 'D-')  {n=0.25}
      else if (letter == 'D/R')  {n=0.2}
      else if (letter == 'R+')  {n=0.15}
      else if (letter == 'R')  {n=0.1}
      else if (letter == 'R-')  {n=0.05}
      else if (letter == 'R--')  {n=0};
	    return n};

     Jupyter.notebook.metadata.grade = {};

     var rubric_categories = Jupyter.notebook.metadata.org.RUBRIC_CATEGORIES.split(", ")
     var rubric_weights = Jupyter.notebook.metadata.org.RUBRIC_WEIGHTS.split(", ")
     grade = 0.0;
     var text = '';

     for (i=0; i < rubric_categories.length; i++) {
	 var promptstring = rubric_categories[i] + ": ";
	 var category_letter_grade = prompt(promptstring).toUpperCase();
	 var category_grade = parseFloat(rubric_weights[i]) * letter_to_number(category_letter_grade);
	 Jupyter.notebook.metadata.grade[rubric_categories[i]] = category_letter_grade
	 grade += category_grade;
	 text += rubric_categories[i].toUpperCase() + ': ' + category_letter_grade + "\n\n";
     }

     text += "Overall: " + String(grade.toFixed(3)) + "\n\n"

     Jupyter.notebook.metadata.grade.overall = grade;

     var lettergrade;
     if (grade == "none") {lettergrade="unfinished"}
     else if (grade == 1.0) {lettergrade="A++"}
     else if (grade >= 0.95) {lettergrade="A+"}
     else if (grade >= 0.90) {lettergrade="A"}
     else if (grade >= 0.85) {lettergrade="A-"}
     else if (grade >= 0.80) {lettergrade="A/B"}
     else if (grade >= 0.75) {lettergrade="B+"}
     else if (grade >= 0.70) {lettergrade="B"}
     else if (grade >= 0.65) {lettergrade="B-"}
     else if (grade >= 0.60) {lettergrade="B/C"}
     else if (grade >= 0.55) {lettergrade="C+"}
     else if (grade >= 0.50) {lettergrade="C"}
     else if (grade >= 0.45) {lettergrade="C-"}
     else if (grade >= 0.40) {lettergrade="C/D"}
     else if (grade >= 0.35) {lettergrade="D+"}
     else if (grade >= 0.30) {lettergrade="D"}
     else if (grade >= 0.25) {lettergrade="D-"}
     else if (grade >= 0.20) {lettergrade="D/R"}
     else if (grade >= 0.15) {lettergrade="R+"}
     else if (grade >= 0.10) {lettergrade="R"}
     else if (grade >= 0.05) {lettergrade="R-"}
     else {lettergrade = "R--"};

     var kernel = Jupyter.notebook.kernel;
     kernel.execute("import getpass; username=getpass.getuser(); print(username)",
		    {iopub: {output: function(response) {
			var username = response.content.text;
			text += "\n\nGraded by: " + username;
			var cells = Jupyter.notebook.get_cells()
			var N = cells.length
			Jupyter.notebook.select(N - 1)
			Jupyter.notebook.insert_cell_below();
			Jupyter.notebook.select_next();
			Jupyter.notebook.edit_mode();

			Jupyter.notebook.get_selected_cell().set_text(text);
			Jupyter.notebook.to_markdown();
			Jupyter.notebook.execute_cell();}}},
		    {silent: false,
		     store_history: false,
		     stop_on_error: true});};

var techela_add_grade_action = {
    help : 'Add grade',
    help_index : 'zz',
    handler : techela_add_grade };

var add_grade = Jupyter.actions.register(techela_add_grade_action, 'Add grade', 'techela');

Jupyter.keyboard_manager.command_shortcuts.add_shortcut('g', add_grade);

// ------------------------------------------------------------------

function techela_save_and_quit (event) {
	Jupyter.notebook.save_notebook();
	Jupyter.notebook.save_checkpoint();
        Jupyter.notebook.session.delete();
	window.close();
    return false; };

var techela_quit_action = {
    help : 'Save, delete session and close the tab.',
    help_index : 'zz',
    handler : save_quit };

var save_quit = Jupyter.actions.register(techela_save_and_quit, 'Save and quit', 'techela');

Jupyter.keyboard_manager.command_shortcuts.add_shortcut('q', save_quit);
