------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                BASE                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.containers;
with ada.exceptions; 			use ada.exceptions;
 
with ada.command_line;			use ada.command_line;
with gnat.command_line;			use gnat.command_line;
with ada.directories;			use ada.directories;

--with gnat.source_info;

with et_modes;					use et_modes;
with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_import;
with et_export;
with et_conventions;
with et_kicad.schematic;
with et_kicad_to_native;
with et_project;
with et_project.modules;
with et_project.rigs;
with et_scripting;

with et_packages;
with et_pcb_rw;
with et_pcb_rw.device_packages;

with et_symbols;
with et_symbol_rw;

with et_devices;				use et_devices;
with et_device_rw;

with et_frames;
with et_frame_rw;

with et_coordinates;
with et_pcb_coordinates;

with et_gui;

procedure et is

	conv_file_name_create	: et_conventions.pac_file_name.bounded_string;
-- 	conv_file_name_use		: et_conventions.pac_file_name.bounded_string;

	project_name_create		: et_project.pac_project_name.bounded_string; -- the project to be created
	project_name_import		: et_project.pac_project_name.bounded_string; -- the project to be imported
	project_name_open 		: et_project.pac_project_name.bounded_string; -- the project to be opened
	project_name_save_as	: et_project.pac_project_name.bounded_string; -- the "save as" name of the project

	module_file_name		: pac_module_file_name.bounded_string;	-- the name of the module file like "motor_driver.mod"
	module_sheet			: et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
	
	package_name_create		: et_packages.pac_package_model_file_name.bounded_string; -- the package to be created like libraries/packages/S_SO14.pac
	package_name_import		: et_packages.pac_package_model_file_name.bounded_string; -- the package to be imported
	package_name_open		: et_packages.pac_package_model_file_name.bounded_string; -- the package to be opened
	package_name_save_as	: et_packages.pac_package_model_file_name.bounded_string; -- the package to be saved as
	package_appearance		: et_packages.type_package_appearance := et_packages.REAL; -- virtual/real. mostly real.

	symbol_name_create		: et_symbols.pac_symbol_model_file.bounded_string; -- the symbol to be created like libraries/symbols/nand.sym
	symbol_name_open		: et_symbols.pac_symbol_model_file.bounded_string; -- the symbol to be opened
	symbol_name_save_as		: et_symbols.pac_symbol_model_file.bounded_string; -- the symbol to be saved as
	symbol_appearance		: et_symbols.type_appearance := et_symbols.PCB; -- virtual/pcb. mostly pcb.
	
	device_name_create		: pac_device_model_file.bounded_string; -- the device to be created like libraries/devices/TL084.dev
	device_name_open		: pac_device_model_file.bounded_string; -- the device to be opened
	device_name_save_as		: pac_device_model_file.bounded_string; -- the device to be saved as
	device_appearance		: et_symbols.type_appearance := et_symbols.PCB; -- virtual/pcb. mostly pcb.

	frame_name_create		: et_frames.pac_template_name.bounded_string; -- the frame to be created like lib/frames/A3_landscape.frs
	frame_name_open			: et_frames.pac_template_name.bounded_string;
	frame_name_save_as		: et_frames.pac_template_name.bounded_string;
	frame_domain			: et_frames.type_domain := et_frames.SCHEMATIC;
	
	script_name				: pac_script_name.bounded_string;
	
	dummy_name : constant string := "dummy";

	message_error : constant string := "ERROR ! ";
	
	procedure get_commandline_arguments is
		use ada.characters.latin_1;
		
		arg : constant string := ("argument: -");
		equals : character renames equals_sign;
	begin
		loop 
			case getopt (switch_version -- FIND THE SWITCH STRINGS IN ET_GENERAL !!!
						& space & switch_help -- no parameter
						& space & switch_make_default_conv & equals
						& space & switch_log_level & equals
						& space & switch_import_project & equals
						& space & switch_import_format & equals

						-- project
						& space & switch_native_project_create & equals
						& space & switch_native_project_open & equals
						& space & switch_native_project_save_as & equals
						& space & switch_native_project_module & equals
						& space & switch_native_project_sheet & equals

						-- package
						& space & switch_native_package_create -- no parameter
						& space & switch_package_appearance & equals
						& space & switch_native_package_open & equals
						& space & switch_native_package_save_as & equals

						-- symbol
						& space & switch_native_symbol_create -- no parameter
						& space & switch_symbol_appearance & equals
						& space & switch_native_symbol_open & equals
						& space & switch_native_symbol_save_as & equals

						-- device
						& space & switch_native_device_create -- no parameter
						& space & switch_device_appearance & equals
						& space & switch_native_device_open & equals
						& space & switch_native_device_save_as & equals

						-- frame schematic
						& space & switch_frame_schematic_create & equals
						& space & switch_frame_schematic_open & equals
						& space & switch_frame_schematic_save_as & equals

						-- frame pcb
						& space & switch_frame_pcb_create & equals
						& space & switch_frame_pcb_open & equals
						& space & switch_frame_pcb_save_as & equals

						-- script
						& space & switch_execute_script & equals

						-- runmode
						& space & switch_runmode & equals
					) is

				when hyphen => -- which is a '-'
					if full_switch = switch_version then
						put_line (system_name & " version " & version);
						
					elsif full_switch = switch_help then
						put_line ("help"); -- CS write helpful help

					elsif full_switch = switch_import_project then
						log (text => arg & full_switch & space & strip_directory_separator (parameter));
						project_name_import := et_project.to_project_name (parameter);

					elsif full_switch = switch_import_format then
						log (text => arg & full_switch & space & parameter);
						et_import.cad_format := et_import.type_cad_format'value (parameter);

					elsif full_switch = switch_make_default_conv then -- make conventions file
						log (text => arg & full_switch & space & parameter);
						conv_file_name_create := et_conventions.to_file_name (parameter);

					-- project
					elsif full_switch = switch_native_project_create then
						log (text => arg & full_switch & space & parameter);
						project_name_create := et_project.to_project_name (remove_trailing_directory_separator (parameter));
						
					elsif full_switch = switch_native_project_open then
						log (text => arg & full_switch & space & parameter);
						project_name_open := et_project.to_project_name (remove_trailing_directory_separator (parameter));

					elsif full_switch = switch_native_project_save_as then
						log (text => arg & full_switch & space & parameter);
						project_name_save_as := et_project.to_project_name (remove_trailing_directory_separator (parameter));

					elsif full_switch = switch_native_project_module then
						log (text => arg & full_switch & space & parameter);
						module_file_name := to_module_file_name (parameter);
						
					elsif full_switch = switch_native_project_sheet then
						log (text => arg & full_switch & space & parameter);
						module_sheet := et_coordinates.to_sheet (parameter);
						
					-- package
					elsif full_switch = switch_native_package_create then
						log (text => arg & full_switch); -- no parameter
						package_name_create := et_packages.to_file_name (dummy_name);

					elsif full_switch = switch_package_appearance then -- virtual/real
						log (text => arg & full_switch & space & parameter);
						package_appearance := et_packages.to_appearance (parameter); -- if not provided -> default used
						
					elsif full_switch = switch_native_package_open then
						log (text => arg & full_switch & space & parameter);
						package_name_open := et_packages.to_file_name (parameter); -- libraries/packages/smd/SOT23.pac

					elsif full_switch = switch_native_package_save_as then
						log (text => arg & full_switch & space & parameter);
						package_name_save_as := et_packages.to_file_name (parameter); -- libraries/packages/smd/SOT23.pac

					-- symbol
					elsif full_switch = switch_native_symbol_create then
						log (text => arg & full_switch); -- no parameter
						symbol_name_create := et_symbols.to_file_name (dummy_name);

					elsif full_switch = switch_symbol_appearance then -- virtual/pcb
						log (text => arg & full_switch & space & parameter);
						symbol_appearance := et_symbols.to_appearance (parameter); -- if not provided -> default used
						
					elsif full_switch = switch_native_symbol_open then
						log (text => arg & full_switch & space & parameter);
						symbol_name_open := et_symbols.to_file_name (parameter); -- libraries/symbols/nand.sym

					elsif full_switch = switch_native_symbol_save_as then
						log (text => arg & full_switch & space & parameter);
						symbol_name_save_as := et_symbols.to_file_name (parameter);


					-- device
					elsif full_switch = switch_native_device_create then
						log (text => arg & full_switch); -- no parameter
						device_name_create := et_devices.to_file_name (dummy_name);

					elsif full_switch = switch_device_appearance then -- virtual/pcb
						log (text => arg & full_switch & space & parameter);
						device_appearance := et_symbols.to_appearance (parameter); -- if not provided -> default used
						
					elsif full_switch = switch_native_device_open then
						log (text => arg & full_switch & space & parameter);
						device_name_open := et_devices.to_file_name (parameter); -- libraries/devices/TL084.dev

					elsif full_switch = switch_native_device_save_as then
						log (text => arg & full_switch & space & parameter);
						device_name_save_as := et_devices.to_file_name (parameter);

						
					-- frame schematic
					elsif full_switch = switch_frame_schematic_create then
						log (text => arg & full_switch & space & parameter);
						frame_name_create := et_frames.to_template_name (parameter);
						frame_domain := et_frames.SCHEMATIC;
						
					elsif full_switch = switch_frame_schematic_open then
						log (text => arg & full_switch & space & parameter);
						frame_name_open := et_frames.to_template_name (parameter);
						frame_domain := et_frames.SCHEMATIC;
						
					elsif full_switch = switch_frame_schematic_save_as then
						log (text => arg & full_switch & space & parameter);
						frame_name_save_as := et_frames.to_template_name (parameter);


					-- frame pcb
					elsif full_switch = switch_frame_pcb_create then
						log (text => arg & full_switch & space & parameter);
						frame_name_create := et_frames.to_template_name (parameter);
						frame_domain := et_frames.PCB;

					elsif full_switch = switch_frame_pcb_open then
						log (text => arg & full_switch & space & parameter);
						frame_name_open := et_frames.to_template_name (parameter);
						frame_domain := et_frames.PCB;
						
					elsif full_switch = switch_frame_pcb_save_as then
						log (text => arg & full_switch & space & parameter);
						frame_name_save_as := et_frames.to_template_name (parameter);
						
					-- script
					elsif full_switch = switch_execute_script then
						log (text => arg & full_switch & space & parameter);
						script_name := to_script_name (parameter);
						
					elsif full_switch = switch_log_level then
						log (text => arg & full_switch & space & parameter);
						log_level := type_log_level_cmd_line'value (parameter);

					-- runmode
					elsif full_switch = switch_runmode then
						log (text => arg & full_switch & space & parameter);
						runmode := to_runmode (parameter);
						
					end if;
					
				when others => exit;

			end case;
		end loop;

		exception
			when others =>
				put_line (message_error & "command line argument error !");
				show_cdl_switches;
				raise;
				
	end get_commandline_arguments;

	procedure backup_projects_root_directory is
		use et_project;
		use et_project.type_projects_root_dir;
	begin
		-- CS: log ?
		projects_root_dir := to_bounded_string (current_directory);
	end;

	procedure restore_projects_root_directory is
		use et_project;
		use et_project.type_projects_root_dir;
	begin
		log_indentation_reset;
		log (text => "changing back to projects directory " & to_string (projects_root_dir) & " ...",
			 level => 1);
		set_directory (to_string (projects_root_dir));
	end;
	
	procedure create_work_directory is
		use et_general;
	begin
		if not exists (work_directory) then
			put_line ("creating " & system_name & " work directory " & work_directory & " ...");
			create_directory (work_directory);
		end if;
	end;

	procedure create_report_directory is begin	
		if not exists (compose (work_directory, report_directory)) then
			put_line ("creating report directory ...");
			create_directory (compose (work_directory, report_directory));
		end if;
	end;

	procedure import_project is -- CS move to et_import ?
	-- As a result of the import, a native project is created in the work_directory (ET/...).
		use et_project.pac_project_name;
		use et_import;
	begin
		-- Test if project name specified and if project base directory exists:
		if length (project_name_import) > 0 then

			-- If project name was provided with a trailing directory separator it must be removed.
-- 			project_name_import := et_project.to_project_name (strip_directory_separator (et_project.to_string (project_name_import)));
			validate_project (project_name_import, et_import.cad_format);
		else
			put_line (message_error & "project name not specified !");
			raise constraint_error;
		end if;

		-- Test if cad format specified:
		if et_import.cad_format = UNKNOWN then
			put_line (message_error & "CAD format not specified !");
			raise constraint_error;
		end if;		

		log (text => "importing project " & et_project.to_string (project_name_import) & " ...", console => true);
		log (text => "CAD format " & to_string (et_import.cad_format));
				
		case et_import.cad_format is
			when et_import.KICAD_V4 | et_import.KICAD_V5 =>

				-- do the import
				et_kicad.schematic.import_design (project => project_name_import, log_threshold => 0);

				-- convert to native project (with a default rig configuration file)
				log (text => et_string_processing.row_separator_single);
				log (text => "converting to " & et_general.system_name & " native project ...", console => true);
				log_indentation_up;

				-- The project will be saved in the current working directory:
				et_kicad_to_native.to_native (et_project.to_project_name
					(base_name (to_string (project_name_import))), log_threshold => 0);
				log_indentation_down;
				
			when others => -- CS
				raise constraint_error;
		end case;
		
		exception
			when event:
				others =>
					put_line (standard_output, message_error & "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;

	end import_project;

	procedure save_package_as is 
		use et_packages;
		use et_packages.pac_package_model_file_name;
	begin
		-- If package_name_save_as is empty nothing happens.
		-- Otherwise the latest and only packagein et_packages.packages is saved.

		-- CS: Test file extension package_model_file_extension
		
		if length (package_name_save_as) > 0 then
			et_pcb_rw.device_packages.save_package (
				file_name 		=> package_name_save_as,
				packge			=> pac_packages_lib.last_element (et_packages.packages_lib),
				log_threshold	=> 0);
		end if;
	end;

	procedure save_symbol_as is 
		use et_symbols.pac_symbol_model_file;
	begin
		-- If symbol_name_save_as is empty nothing happens.
		-- Otherwise the latest and only symbol in et_symbols.symbols is saved.
		if length (symbol_name_save_as) > 0 then
			et_symbol_rw.save_symbol (
				file_name 		=> symbol_name_save_as,
				symbol			=> et_symbols.pac_symbols.last_element (et_symbols.symbols),
				log_threshold	=> 0);
		end if;
	end;

	procedure save_device_as is 
		use pac_device_model_file;
	begin
		-- If device_name_save_as is empty nothing happens.
		-- Otherwise the latest and only device in et_devices.devices is saved.
		if length (device_name_save_as) > 0 then
			et_device_rw.save_device (
				file_name 		=> device_name_save_as,
				device			=> et_devices.pac_devices_lib.last_element (et_devices.devices),
				log_threshold	=> 0);
		end if;
	end;

	procedure launch_schematic_and_board_editor is 
		use ada.containers;
		use et_gui;
		use et_project.modules;
		use et_project.modules.pac_generic_modules;
		use pac_module_file_name;
		use pac_module_name;

		generic_module_name : et_general.pac_module_name.bounded_string;
		module_cursor : pac_generic_modules.cursor;
		script_name_tmp : pac_script_name.bounded_string;
	begin
		-- If no generic modules available at all, create an untitled module:
		if length (generic_modules) = 0 then
			
			create_module (
				module_name		=> to_module_name (untitled),
				log_threshold	=> 0);
			-- NOTE: does not create a module file as this module is still untitled.
			
			module_cursor := generic_modules.first; -- select the untitled generic module
		else
		-- Generic module are available:
			
			-- If no module name was given via command line, then the first
			-- available generic module will be opened.
			if length (module_file_name) = 0 then
				module_cursor := generic_modules.first; -- select first available generic module
			else
				-- Convert the optionally given module file name to a module name.
				generic_module_name := to_module_name (remove_extension (
					simple_name (pac_module_file_name.to_string (module_file_name))));
				
				module_cursor := find (generic_modules, generic_module_name);
			end if;
		end if;
			
		-- The script name must be passed to gui.single_module as simple name like rename_nets.scr.
		-- So we render something like motor_driver/rename_nets.scr to just rename_nets.scr:
		if pac_script_name.length (script_name) > 0 then
			script_name_tmp := to_script_name (simple_name (to_string (script_name))); -- rename_nets.scr
		end if;
		
		-- We pass the script name (even if empty) to the schematic so
		-- that it gets executed from there. If the script name is empty,
		-- no script will be executed by the gui.
		single_module (
			project			=> project_name_open,	-- blood_sample_analyzer
			module			=> module_cursor,		-- cursor to generic module
			sheet			=> module_sheet, 		-- 1, 3, 10, ... as given via cmd line
			script			=> script_name_tmp,
			log_threshold	=> 0);
		
	end launch_schematic_and_board_editor;

	procedure process_commandline_arguments is
		use et_project.pac_project_name;
		use pac_script_name;
		use et_conventions.pac_file_name;
		use et_packages.pac_package_model_file_name;
		use et_symbols.pac_symbol_model_file;
		use pac_device_model_file;
		use et_frames.pac_template_name;

		exit_code_script : et_scripting.type_exit_code;
		
	begin -- process_commandline_arguments
		-- The arguments are processed according to a certain priority.
		-- 1. create conventions file
		-- 2. import foreign project
		-- 3. open native project
		-- 3.1. execute script on native project

		-- If the operator wishes to create a conventions file it will be done.
		-- Other command line parameters are ignored:
		if length (conv_file_name_create) > 0 then
			et_conventions.make_default_conventions (conv_file_name_create, log_threshold => 0);
		else
			-- If operator wants to create a new project it will be created in the current directory:
			if length (project_name_create) > 0 then

				-- create project directory
				runmode := MODE_HEADLESS;
				
				-- CS: provide module name via cdl argument
				et_project.create_project_directory (
					module_name		=> to_module_name (to_string (project_name_create)),
					project_name	=> project_name_create,
-- 					project_path	=> et_project.to_project_path (""),
					log_threshold	=> 0);
				
			-- If operator wants to import a project it will be done here.
			elsif length (project_name_import) > 0 then
				import_project;

			-- Otherwise a native project will be opened:
			elsif length (project_name_open) > 0 then

				et_project.open_project (project_name_open, log_threshold => 0);

				-- If operator whishes to execute a script on the native project:
				if length (script_name) > 0 then

					-- NOTE: In headless mode the script will be executed right here.
					-- Function et_scripting.execute_script parses the script line per line
					-- and calls procedure et_scripting.execute_command for each line.
					
					-- In graphical mode (means everything other than MODE_HEADLESS)
					-- the script will NOT be executed here but FROM INSIDE the GUI 
					-- as if it where an ordinary command entered by the operator.
					-- See procedure et_gui.single_module for more.
					
					case runmode is
						when MODE_HEADLESS =>

							--cmd_entry_mode := SCRIPT_ON_STARTUP;
							
							exit_code_script := et_scripting.execute_script (script_name, log_threshold => 0);

							-- evaluate exit code
							case exit_code_script is
								when et_scripting.ERROR =>
									log (ERROR, "Execution of script " & et_general.to_string (script_name) &
										" failed !", console => true);
									raise constraint_error;

								when et_scripting.WARNINGS =>
									log (WARNING, "Execution of script " & et_general.to_string (script_name) &
										" produced warnings !", console => true);

								when et_scripting.SUCCESSFUL =>
									log (text => "Execution of script " & et_general.to_string (script_name) & " successful");
									
							end case;

						when others => null;
					end case;
					
				end if;
				
				-- optionally the project can be saved with a different name
				if length (project_name_save_as) > 0 then
					et_project.save_project (project_name_save_as, log_threshold => 0);
				end if;

				
			-- package
			elsif length (package_name_create) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for package editing
				
				et_pcb_rw.device_packages.create_package (package_name_create, package_appearance, log_threshold => 0);

				-- optionally the package can be saved under a different name
				save_package_as;  -- if package_name_save_as is empty nothing happens

			elsif length (package_name_import) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for package editing
				null; -- CS

			elsif length (package_name_open) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for package editing
				
				et_pcb_rw.device_packages.read_package (package_name_open, log_threshold => 0);

				-- optionally the package can be saved under a different name
				save_package_as; -- if package_name_save_as is empty nothing happens

				
			-- symbol
			elsif length (symbol_name_create) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for symbol editing
				
				et_symbol_rw.create_symbol (symbol_name_create, symbol_appearance, log_threshold => 0);

				-- optionally the symbol can be saved under a different name
				save_symbol_as; -- if symbol_name_save_as is empty nothing happens

			elsif length (symbol_name_open) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for symbol editing
				
				et_symbol_rw.read_symbol (symbol_name_open, log_threshold => 0);

				-- optionally the symbol can be saved under a different name				
				save_symbol_as; -- if symbol_name_save_as is empty nothing happens


			-- device
			elsif length (device_name_create) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for device editing
				
				et_device_rw.create_device (device_name_create, device_appearance, log_threshold => 0);

				-- optionally the device can be saved under a different name
				save_device_as; -- if device_name_save_as is empty nothing happens

			elsif length (device_name_open) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for device editing
				
				et_device_rw.read_device (device_name_open, log_threshold => 0);

				-- optionally the device can be saved under a different name				
				save_device_as; -- if device_name_save_as is empty nothing happens


			-- frame
			elsif length (frame_name_create) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for frame editing
				
				et_frame_rw.create_frame (frame_name_create, frame_domain, log_threshold => 0); -- incl. save to file

			elsif length (frame_name_open) > 0 then
				runmode := MODE_HEADLESS; -- CS as long as there is no GUI for frame editing
				
				declare
					use et_frames;
					use et_frame_rw;
					frame : type_frame (frame_domain);
				begin
					frame := read_frame (frame_name_open, frame_domain, log_threshold => 0);

					-- optionally the framc can be saved under a different name				
					if length (frame_name_save_as) > 0 then
						save_frame (frame, frame_name_save_as, log_threshold => 0);
					end if;
				end;

			end if;
			
		end if;
		
	end process_commandline_arguments;


	procedure log_sys_info is 
	begin
		log (text => et_pcb_coordinates.pac_geometry_brd.get_info ("layout/board"));
		log (text => et_coordinates.pac_geometry_sch.get_info ("schematic"));
	end log_sys_info;


	
begin -- main

	if argument_count = 0 then
		-- If operator does not provide any arguments, show possible options.
		show_cdl_switches;
		
	else
		
		-- create a directory where imported projects and reports live:
		create_work_directory;

		-- create inside the work directory another directory for reports and log messages:
		create_report_directory;

		create_report;

		log_sys_info;
		
		get_commandline_arguments;

		process_commandline_arguments;

		case runmode is
			when MODE_HEADLESS => null;
			when MODE_MODULE => launch_schematic_and_board_editor;
			when others => null;
		end case;
		
		close_report;
	end if;
	
	exception
		when event: others =>
			log_indentation_reset;
			log (text => ada.exceptions.exception_information (event), console => true);
			close_report;
			
			put_line ("Read log file " & log_file_name & " for details !");
			set_exit_status (failure);

end et;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
