## No-code APP

## https://cygubicko.github.io/varpred/index.html 

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

autopipeR = defined

######################################################################

## Anything to be added is included in Sources += 

Sources += $(wildcard *.md)
Sources += $(wildcard *.R R/*.R)
Sources += $(wildcard js/*.js)
Sources += $(wildcard WWW/*.js)
Sources += $(wildcard server/*.R)
Sources += $(wildcard ui/*.R)
Sources += $(wildcard static_files/*.*)
Sources += LICENSE

Ignore += datasets/*

######################################################################

## Application

ui.Rout: ui.R
server.Rout: server.R

######################################################################

## UI side

# upload_data.Rout: ui/upload_data.R
# upload_form.Rout: ui/upload_form.R

## Theme
appTheme.Rout: ui/appTheme.R
header.Rout: ui/header.R
footer.Rout: ui/footer.R
headertag.Rout: ui/headertag.R
dashboard_body.Rout: ui/dashboard_body.R

######################################################################

## Server side

#### Header and footer details 
header_footer_configs.Rout: server/header_footer_configs.R

#### Create directories
create_dirs.Rout: server/create_dirs.R

### Change language
change_language.Rout: server/change_language.R

### Validate inputs
input_validators.Rout: server/input_validators.R

### Input files/datasets
input_files.Rout: server/input_files.R

### Upload data
upload_data.Rout: server/upload_data.R

### Transform data
database_integration.Rout: server/database_integration.R

### Display uploaded data
display_uploaded_data.Rout: server/display_uploaded_data.R

### Delete uploaded data
delete_uploaded_data.Rout: server/delete_uploaded_data.R

### Display uploaded data
display_metadata.Rout: server/display_metadata.R

### Collect logs
collect_logs.Rout: server/collect_logs.R

### Update logs
update_logs.Rout: server/update_logs.R

### Select data
select_data.Rout: server/select_data.R

### Selected data
selected_data.Rout: server/selected_data.R

### Manage data summary
manage_data_summary.Rout: server/manage_data_summary.R

### Explore data
explore_data.Rout: server/explore_data.R

### Transform data
transform_data.Rout: server/transform_data.R

### Combine data
combine_data.Rout: server/combine_data.R

### Research questions
research_questions.Rout: server/research_questions.R

### Reset inputs
resets.Rout: server/resets.R

### Helper files
shinyutilities.Rout: R/shinyutilities.R

## All make rules goes here 
runapp:
	echo "shiny::runApp(\".\", port=3001)" | R --slave

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/chains.mk
-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
