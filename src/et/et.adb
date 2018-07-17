------------------------------------------------------------------------------
--                                                                          --
--                            SYSTEM ET BASE                                --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your editor to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.integer_text_io;		use ada.integer_text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.exceptions; 			use ada.exceptions;
 
with ada.command_line;			use ada.command_line;
with gnat.command_line;			use gnat.command_line;
with ada.directories;			use ada.directories;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_coordinates;
with et_schematic;
with et_import;
with et_export;
with et_configuration;
with et_kicad;
with et_kicad_pcb;
with et_project;

procedure et is

	-- Depending on the command line arguments this variable tells what the operator wants to do:
	operator_action : type_operator_action := request_help;
	conf_file_name	: et_configuration.type_configuration_file_name.bounded_string;	

	project_name : et_schematic.type_project_name.bounded_string; -- used for single module import
	
	procedure get_commandline_arguments is
		use et_schematic;
	begin
		loop 
			case getopt (switch_version 
						& latin_1.space & switch_help -- no parameter
						& latin_1.space & switch_make_default_conf & latin_1.equals_sign
						& latin_1.space & switch_log_level & latin_1.equals_sign
						--& latin_1.space & switch_import_file & latin_1.equals_sign -- CS: see below
						& latin_1.space & switch_import_module & latin_1.equals_sign
						& latin_1.space & switch_import_format & latin_1.equals_sign
						& latin_1.space & switch_import_modules -- no parameter
						& latin_1.space & switch_configuration_file & latin_1.equals_sign
					) is

				when latin_1.hyphen => -- which is a '-'
					if full_switch = switch_version then
						put_line (system_name & " version " & version);
						
					elsif full_switch = switch_help then
						put_line ("help"); -- CS write helpful help

--					CS: currently we do not care about importing single files
--						needs distinction between schematic and board file
-- 					elsif full_switch = switch_import_file then
-- 						put_line("import file " & parameter); 

					elsif full_switch = switch_import_module then
						put_line ("import module " & strip_directory_separator (parameter));
						project_name := type_project_name.to_bounded_string (parameter);

						-- set operator action
						operator_action := import_module;

					elsif full_switch = switch_import_format then
						put_line ("import format " & parameter);
						et_import.cad_format := et_import.type_cad_format'value (parameter);

					elsif full_switch = switch_import_modules then
						put_line ("import modules as specified by configuraton file");

						-- set operator action
						operator_action := import_modules;
						
					elsif full_switch = switch_make_default_conf then -- make configuration file
						put_line ("configuration file " & parameter);

						-- set operator action
						operator_action := make_configuration;
						conf_file_name := et_configuration.type_configuration_file_name.to_bounded_string (parameter);

					elsif full_switch = switch_configuration_file then -- define configuration file
						put_line ("configuration file " & parameter);
						conf_file_name := et_configuration.type_configuration_file_name.to_bounded_string (parameter);
						
					elsif full_switch = switch_log_level then
						put_line ("log level " & parameter);
						log_level := type_log_level_cmd_line'value (parameter);
					end if;

					
				when others => exit; -- CS: produce useful message

			end case;
		end loop;

		exception
			when others =>
				put_line (message_error & "command line argument error !");
				null;
				-- CS: show help and command line switches
				raise;
				
	end get_commandline_arguments;

	procedure backup_projects_root_directory is
		use et_schematic;
		use et_schematic.type_projects_root_dir;
	begin
		-- CS: log ?
		projects_root_dir := to_bounded_string (current_directory);
	end backup_projects_root_directory;

	procedure restore_projects_root_directory is
		use et_schematic;
		use et_schematic.type_projects_root_dir;
	begin
		log_indentation_reset;
		log (text => "changing back to projects directory " & to_string (projects_root_dir) & " ...",
			 level => 1);
		set_directory (to_string (projects_root_dir));
	end restore_projects_root_directory;
	
	procedure create_work_directory is
	begin
		if not exists (work_directory) then
			put_line ("creating " & system_name & " work directory " & work_directory & " ...");
			create_directory (work_directory);
		end if;
	end create_work_directory;

	procedure create_report_directory is
	begin	
		if not exists (compose (work_directory, report_directory)) then
			put_line ("creating report directory ...");
			create_directory (compose (work_directory, report_directory));
		end if;
	end create_report_directory;
	
	procedure import_module is
	-- This imports a single module.
	-- CAUTION: uses the global variable project_name !!!
		use et_schematic;
		use et_schematic.type_project_name;
		use et_import;
	begin
		-- Test if project name specified and if project base directory exists:
		if length (project_name) > 0 then

			-- If project name was provided with a trailing directory separator it must be removed.
			project_name := to_bounded_string (strip_directory_separator (et_schematic.to_string (project_name)));
			validate_project (project_name, et_import.cad_format);
		else
			put_line (message_error & "project name not specified !");
			raise constraint_error;
		end if;

		-- Test if cad format specified:
		if et_import.cad_format = UNKNOWN then
			put_line (message_error & "CAD format not specified !");
			raise constraint_error;
		end if;		

		create_work_directory;
		create_report_directory;
		et_import.create_report; -- directs all puts to the report file

		-- read configuration file if specified. otherwise issue warning
		if et_configuration.type_configuration_file_name.length (conf_file_name) > 0 then
			et_configuration.read_configuration (
				file_name => conf_file_name,
				single_module => true, -- we are dealing a single project
				log_threshold => 0);
		else
			log (message_warning & "no configuration file specified !");
		end if;
		
		-- The design import requires changing of directories. So we backup the current directory.
		-- After the import, we restore the directory.
		backup_projects_root_directory;

		log ("importing module " & et_schematic.to_string (project_name) & " ...");
		log ("CAD format " & to_string (et_import.cad_format));
		
		-- CS: use case construct to probe cad formats
		et_kicad.import_design (project => project_name, log_threshold => 0);
		restore_projects_root_directory;

		--et_import.close_report;
		-- keep import report open for things that follow later (for example layout import)
		set_output (standard_output);

		exception
			when event:
				others =>
					et_import.close_report;
					put_line (standard_output, message_error & "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;

	end import_module;


	procedure import_modules is
	-- Imports modules as specified in configuration file and inserts them in the rig.
		use et_schematic;
		use et_schematic.type_project_name;
		use et_configuration;
		use type_import_modules;
		use et_coordinates;

		module_cursor_import : type_import_modules.cursor;
		instances : type_submodule_instance;
		module : type_project_name.bounded_string; -- when importing multiple projects, we regard them as modules
	begin
		log ("importing modules ...");
		
		create_work_directory;
		create_report_directory;
		et_import.create_report; -- directs all puts to the report file

		-- Read configuration file if specified. otherwise issue warning 
		-- CS: make a procedure as this is used by procedure import_desing too.
		if et_configuration.type_configuration_file_name.length (conf_file_name) > 0 then
			et_configuration.read_configuration (
				file_name => conf_file_name,
				single_module => false, -- we are dealing with more than one module
				log_threshold => 0);
		else
			log (message_warning & "no configuration file specified !");
		end if;
		
		-- Loop in et_configuration.import_module and import module per module.
		module_cursor_import := et_configuration.import_modules.first;
		while module_cursor_import /= type_import_modules.no_element loop

			-- The design import requires changing of directories. So we backup the current directory.
			-- After the import, we restore the directory.
			backup_projects_root_directory;

			module := to_bounded_string (to_string (element (module_cursor_import).name));
			et_import.cad_format := element (module_cursor_import).format;
			instances := element (module_cursor_import).instances;

			-- If the design is to be instantiated multiple times we import only the first instance.
			-- All other instances are created by copying the latest instance.

			if instances = type_submodule_instance'first then 
				-- Only one instance requried -> do a regular single design import.
				log ("importing module " & et_schematic.to_string (module) & " ...");
				log ("CAD format " & et_import.to_string (et_import.cad_format));
				
				-- CS: use case construct to probe cad formats
				et_kicad.import_design (project => module, log_threshold => 0);

			else -- multi-instances
				log ("importing and instantiating module " & et_schematic.to_string (module) & " ...");
				log ("CAD format " & et_import.to_string (et_import.cad_format));
				
				-- Import the project only once.
				for i in type_submodule_instance'first .. instances loop
					log ("instance " & to_string (i) & " ...");

					if i = type_submodule_instance'first then -- first instance
						
						-- CS: use case construct to probe cad formats
						et_kicad.import_design (first_instance => true, project => module, log_threshold => 0);
					else
						-- Copy the last module.
						-- The module instance is incremented by copy_module automatically.
						et_kicad.copy_module (log_threshold => 0);
					end if;
				end loop;

			end if;
			
			restore_projects_root_directory;
			
			next (module_cursor_import);
		end loop;
		
		--et_import.close_report;
		-- keep import report open for things that follow later (for example layout import)
		set_output (standard_output);

		exception
			when event:
				others =>
					et_import.close_report;
					--put (exception_message (event));
					put_line (standard_output, message_error & "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;

	end import_modules;


	procedure check_modules is
	-- This can be regarded as a kind of extended electrical rule check (ERC).
	-- Updates the netlist of ALL modules.	
		use et_schematic;
		use type_rig;
		use et_configuration;
	begin
		-- export useful things from the imported modules
		et_export.create_report;
		--reset_warnings_counter;

		-- If there are no modules, there is nothing to check:
		if et_kicad.module_count > 0 then
		
			log ("checking modules ...", console => true);
			log_indentation_up;
			
			-- detect missing or orphaned junctions
			et_kicad.check_junctions (log_threshold => 0);
			et_kicad.check_orphaned_junctions (log_threshold => 0);
			et_kicad.check_misplaced_junctions (log_threshold => 0);	

			-- detect misplaced no-connect-flags
			et_kicad.check_misplaced_no_connection_flags (log_threshold => 0);

			-- detect orphaned no-connect-flags
			et_kicad.check_orphaned_no_connection_flags (log_threshold => 0);

			-- make netlists
			et_kicad.make_netlists (log_threshold => 0);

			-- detect unintentionally left open ports (must happen AFTER make_netlists !)
			et_kicad.check_open_ports (log_threshold => 0);

			-- detect non-deployed units
			et_kicad.check_non_deployed_units (log_threshold => 0);

			-- test nets for inputs, outputs, bidirs, ...
			et_kicad.net_test (log_threshold => 0);
			
			-- export netlists (requires that make_netlists has been called previously)
			et_kicad.export_netlists (log_threshold => 0);
			
			-- export statistics
			et_kicad.write_statistics (log_threshold => 0);

			-- export bom
			export_bom (log_threshold => 0);
			


			-- If there is more than one module, interconnections must be validated 
			-- as specified in configuration file.
			if et_kicad.module_count > 1 then
				validate_module_interconnections (log_threshold => 0);
			end if;

			-- Create routing tables.
			-- Even if there is just a single module, a routing table is useful.
			make_routing_tables (log_threshold => 0);

			export_routing_tables (log_threshold => 0);
			
			log_indentation_down;

		else
			log ("no modules -> nothing to check and nothing to export");

			-- CS: remove stale directories and files from earlier imports
			-- CS: remove all directories in folder ET (except reports)
			-- CS: remove routing table in ET/reports
		end if;
		
		--et_export.close_report;
		-- CS might be good to leave the export report open for other things that follow (layout export in native format)
	
		exception
			when event:
				others => 
					et_export.close_report;
					put_line (standard_output, message_error & "Read export report for warnings and error messages !"); -- CS: show path to report file
					raise;
				
	end check_modules;


	procedure read_boards is
		use et_schematic;
		use type_rig;
		use et_configuration;
	begin
		-- Log messages go in the import report:
		set_output (et_import.report_handle);
		
		-- If there are no modules, there is nothing to check:
		if et_kicad.module_count > 0 then
		
			log ("importing layouts/boards ...", console => true);
			log_indentation_up;

			-- CS: use case construct to probe cad formats
			et_kicad_pcb.read_boards (log_threshold => 0);
			
			log_indentation_down;
		end if;

		et_import.close_report;
		-- CS might be good to leave the import report open for other things that follow
		
	end read_boards;


	
begin -- main

	-- process command line arguments
	get_commandline_arguments;

	case operator_action is
		when request_help =>
			null; -- CS

		when make_configuration =>
			et_configuration.make_default_configuration (conf_file_name, log_threshold => 0);


			
		when import_module =>
	
			-- import a single module indicated by variable project_name
			import_module; -- calls import_design (according to CAD format)

			-- check the imported module
			check_modules; -- updates the netlists of all modules. creates and opens export report

			read_boards; -- writes in import report. closes import report

			-- Log messages go in the export report:
			set_output (et_export.report_handle);


			-- In directory work_directory/et_project.directory_et_import two sub-directories
			-- are to be created: One for libraries (named by directory_et_import) and another
			-- for the actual project (same name as project itself):

			-- CS clean up previous imports ?
			
			-- create a new libraries directory for components
			et_project.create_libraries_directory_components (
				project_path	=> et_project.type_et_project_path.to_bounded_string (
									compose (work_directory, et_project.directory_import)),
				log_threshold 	=> 0);
			
			-- create a new ET project
			-- It is to be named after the single project that has just been imported.
			et_project.create_project_directory (
				project_name	=> et_project.type_et_project_name.to_bounded_string (et_schematic.to_string (project_name)),
				project_path	=> et_project.type_et_project_path.to_bounded_string (
									compose (work_directory, et_project.directory_import)),
				log_threshold 	=> 0);

			-- write the ET native component libraries			
			et_project.write_component_libraries (log_threshold => 0);
			
			et_export.close_report;

			
		when import_modules =>

			-- import many modules as specified in configuration file
			import_modules; -- calls import_design (according to CAD format)

			-- check modules
			check_modules; -- updates the netlists of all modules. creates and opens export report

			read_boards; -- writes in import report. closes import report

			-- Log messages go in the export report:
			set_output (et_export.report_handle);
			
			-- In directory work_directory/et_project.directory_et_import two sub-directories
			-- are to be created: One for libraries (named by directory_et_import) and another
			-- for the actual project (same name as project itself):

			-- CS clean up previous imports ?
			
			-- create a new libraries directory for components
			et_project.create_libraries_directory_components (
				project_path	=> et_project.type_et_project_path.to_bounded_string (
									compose (work_directory, et_project.directory_import)),
				log_threshold 	=> 0);
			
			-- create a new ET project
			-- CS: It is to be named after the rig name passed as argument. currently statically set to "rig"
			et_project.create_project_directory (
				project_name	=> et_project.type_et_project_name.to_bounded_string ("rig"),
				project_path	=> et_project.type_et_project_path.to_bounded_string (
									compose (work_directory, et_project.directory_import)),
				log_threshold 	=> 0);

			-- write the ET native component libraries			
			et_project.write_component_libraries (log_threshold => 0);
			
			et_export.close_report;
	end case;



	exception
		when event:
			others => 
				log (ada.exceptions.exception_message (event), console => true);
				et_import.close_report;
				et_export.close_report;
				set_exit_status (failure);

end et;

-- Soli Deo Gloria
