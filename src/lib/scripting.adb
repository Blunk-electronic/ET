------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             SCRIPTING                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	--use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with et_project;

with et_coordinates;
with et_pcb_coordinates;
with et_schematic;
with schematic_ops;
with et_packages;
with et_pcb;
with et_pcb_stack;
with board_ops;

with submodules;
with assembly_variants;
with pick_and_place;
with material;
with netlists;
with et_geometry;		use et_geometry; -- due to frequently used keywords
with et_symbols;
with et_devices;		use et_devices;
with et_display;		use et_display;
with et_display.schematic;
with et_display.board;
with glib;
with gtk.main;

package body scripting is

	function to_string (domain : in type_domain) return string is 
	-- Removes the domain_prefix from a domain name and returns the remainder as string.
	-- DOM_PACKAGE becomes PACKAGE.
		s : string := type_domain'image (domain);
	begin
		return s (domain_prefix'length + 1 .. s'last);
	end;

	function to_domain (domain : in string) return type_domain is begin
	-- Prepends the domain_prefix to the given string and returns a type_domain.
	-- PACKAGE becomes DOM_PACKAGE.
		return type_domain'value (domain_prefix & domain);

		exception when event: others => 
			log (ERROR, "domain " & enclose_in_quotes (domain) & " invalid !", console => true);
			raise;
	end;

	function to_string (verb : in type_verb_project) return string is 
		s : string := type_verb_project'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_verb (verb : in string) return type_verb_project is begin
		return type_verb_project'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;
	
	function to_string (noun : in type_noun_project) return string is 
		s : string := type_noun_project'image (noun);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_noun (noun : in string) return type_noun_project is begin
		return type_noun_project'value (noun_prefix & noun);
	
		exception when event: others => 
			log (ERROR, "noun " & enclose_in_quotes (noun) & " invalid !", console => true);
			raise;
	end;
	
	function to_string (verb : in type_verb_board) return string is 
	-- Removes the verb_prefix from verb and returns the remainder as string.
	-- VERB_ADD becomes ADD.
		s : string := type_verb_board'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_verb (verb : in string) return type_verb_board is begin
	-- Prepends the verb_prefix to the given string and returns a type_verb_board.
	-- ADD becomes VERB_ADD.
		return type_verb_board'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;
	
	function to_string (verb : in type_verb_schematic) return string is 
	-- Removes the domain_prefix from verb and returns the remainder as string.
	-- VERB_ADD becomes ADD.
		s : string := type_verb_schematic'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_verb (verb : in string) return type_verb_schematic is begin
	-- Prepends the verb_prefix to the given string and returns a type_verb_schematic.
	-- ADD becomes VERB_ADD.
		return type_verb_schematic'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;
	
	function to_string (noun : in type_noun_schematic) return string is 
		s : string := type_noun_schematic'image (noun);
	begin
		return s (noun_prefix'length + 1 .. s'last);
	end;

	function to_noun (noun : in string) return type_noun_schematic is begin
		return type_noun_schematic'value (noun_prefix & noun);
		exception when event: others => 
			log (ERROR, "noun " & enclose_in_quotes (noun) & " invalid !", console => true);
			raise;
	end;

	function to_string (noun : in type_noun_board) return string is 
		s : string := type_noun_board'image (noun);
	begin
		return s (noun_prefix'length + 1 .. s'last);
	end;

	function to_noun (noun : in string) return type_noun_board is begin
		return type_noun_board'value (noun_prefix & noun);
	
		exception when event: others => 
			log (ERROR, "noun " & enclose_in_quotes (noun) & " invalid !", console => true);
			raise;
	end;

	procedure expect_number (field : in count_type) is begin
		log (ERROR, "number expected in field no." & count_type'image (field) & " !", console => true);
		raise constraint_error;
	end;

	procedure expect_fill_style (style : in et_packages.type_fill_style; field : in count_type) is begin
		log (ERROR, "fill style " & enclose_in_quotes (et_packages.to_string (style)) &
			 " expected in field no. " & count_type'image (field) & " !" , console => true);
		raise constraint_error;
	end;

	procedure invalid_keyword (field : in count_type) is begin
		log (ERROR, "invalid keyword in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	procedure expect_value_center_x (field : in count_type) is begin
		log (ERROR, "Expect value for center x in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	procedure expect_keyword_filled (field : in count_type) is begin
		log (ERROR, "Expect keyword " & enclose_in_quotes (et_geometry.keyword_filled) &
			" in field no." & count_type'image (field) & " !",
			 console => true);
		raise constraint_error;
	end;

	procedure invalid_noun (noun : in string) is begin
		log (ERROR, "invalid noun " & enclose_in_quotes (noun) & " for this operation !",
				console => true);
		raise constraint_error;
	end;

	procedure command_incomplete (cmd : in type_fields_of_line) is begin
		log (ERROR, "command " & enclose_in_quotes (to_string (cmd)) &
			" not complete !", console => true);
		raise constraint_error;
	end;

	procedure command_too_long (
		cmd		: in type_fields_of_line;
		from	: in count_type) 
	is begin
		log (WARNING, "command " & enclose_in_quotes (to_string (cmd)) & " too long !",
			 console => true);
		log (text => " -> Excessive arguments after no." & count_type'image (from) & " ignored !",
			 console => true);
	end;

	procedure validate_module_name (module : in type_module_name.bounded_string) is 
		use et_project;
	begin
		if not exists (module) then
			log (ERROR, "module " & to_string (module) &
				" not found !", console => true);
			raise constraint_error;
		end if;
	end;


-- CANVAS

	function to_string (verb : in type_verb_canvas) return string is 
		s : constant string := type_verb_canvas'image (verb);
	begin
		return s (verb_prefix'length + 1 .. s'last);
	end;

	function to_verb (verb : in string) return type_verb_canvas is begin
		return type_verb_canvas'value (verb_prefix & verb);
	
		exception when event: others => 
			log (ERROR, "verb " & enclose_in_quotes (verb) & " invalid !", console => true);
			raise;
	end;

	function to_string (noun : in type_noun_canvas) return string is 
		s : constant string := type_noun_canvas'image (noun);
	begin
		return s (noun_prefix'length + 1 .. s'last);
	end;

	function to_noun (noun : in string) return type_noun_canvas is begin
		return type_noun_canvas'value (noun_prefix & noun);
	
		exception when event: others => 
			log (ERROR, "noun " & enclose_in_quotes (noun) & " invalid !", console => true);
			raise;
	end;

	procedure terminate_main is begin
		log_indentation_reset;
		log (text => "exiting ...", console => true);
		gtk.main.main_quit;
	end;

	
	procedure execute_nested_script (
		file			: in string;
		log_threshold	: in et_string_processing.type_log_level) is
		
		use et_string_processing;
		use ada.directories;
		use et_canvas_schematic;

		-- Backup previous input:
		previous_input : ada.text_io.file_type renames current_input;
		
		file_handle : ada.text_io.file_type;

		-- The line read from the script:
		line : et_string_processing.type_fields_of_line;

		-- The exit code will be overridden with ERROR or WARNING if something goes wrong:
		exit_code : type_exit_code := SUCCESSFUL;

		script_name : pac_script_name.bounded_string := to_script_name (file);
	begin
		log (text => "current directory: " & enclose_in_quotes (current_directory), level => log_threshold);
		log (text => "executing project internal script: " & enclose_in_quotes (to_string (script_name)), level => log_threshold);
		
		log_indentation_up;
		
		-- make sure the script file exists:
		if exists (to_string (script_name)) then

			-- open script file
			open (
				file => file_handle,
				mode => in_file, 
				name => to_string (script_name)); -- demo.scr

			set_input (file_handle);
			
			-- read the file line by line
			while not end_of_file loop
				
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line,
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then

					-- execute the line as command
					exit_code := execute_command (script_name, line, log_threshold + 1);

					-- evaluate exit code and do things necessary (abort, log messages, ...)
					case exit_code is
						when ERROR => raise constraint_error;
						when others => null;
					end case;
					
				end if;
			end loop;

			--log (text => "closing script file " & enclose_in_quotes (to_string (script_name)), level => log_threshold + 1);
			close (file_handle);
			
		else -- script file not found
			log (ERROR, "script file " & enclose_in_quotes (to_string (script_name)) &
				" not found !", console => true);
			raise constraint_error;
		end if;

		-- A script can be called from inside a script (nested scripts).
		-- When the top level script finishes then there might be no previous
		-- input to switch to. So we test whether the previous_input is open
		-- before swtiching back to it:
		if is_open (previous_input) then
			--log (text => "reset to previous input " & name (previous_input) & " ...", level => log_threshold + 1);
			set_input (previous_input);
		end if;
		
		log_indentation_down;

		exception when event: others =>
			log (
				importance => ERROR,
				text => "Execution of project internal script " 
					& enclose_in_quotes (to_string (script_name))
					& " failed !",
				level => log_threshold + 1,
				console => true
				);
		
			if is_open (file_handle) then close (file_handle); end if;
			if is_open (previous_input) then set_input (previous_input); end if;

	end execute_nested_script;

	
	
	function schematic_cmd (
		cmd				: in type_fields_of_line; -- "schematic motor_driver draw net motor_on 1 150 100 150 130"
		log_threshold	: in type_log_level)
		return type_exit_code is

		use et_project;
		use schematic_ops;
		use et_coordinates;
		use geometry;
		use et_devices;
		use et_canvas_schematic.pac_canvas;
		use et_display.schematic;

		-- The exit code will be overridden with ERROR or WARNING if something goes wrong:
		exit_code : type_exit_code := SUCCESSFUL;
		
		domain	: type_domain; -- DOM_SCHEMATIC
		module	: type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		
		verb : type_verb_schematic;
		noun : type_noun_schematic;

		
		function f (place : in positive) return string is begin
			return et_string_processing.field (cmd, place);
		end;

		function fields return count_type is begin
			return et_string_processing.field_count (cmd);
		end;

		procedure too_long is begin -- CS use it more often
			command_too_long (cmd, fields - 1);
		end;

		procedure zoom_center is -- GUI related
			-- Build the center point:
			c : type_point := type_point (set (
					x => to_distance (f (5)),
					y => to_distance (f (6))));
		begin
			log (text => "center on point", level => log_threshold + 1);
			center_on (canvas, c);
		end zoom_center;

		procedure set_scale (scale : in string) is  -- GUI related -- CS should be percent of scale_to_fit
			use glib;
			s : gdouble := gdouble'value (scale);
		begin
			log (text => "zoom level", level => log_threshold + 1);
			set_scale (canvas, s);
		end set_scale;

		-- Positions the cursor absolute or relative:
		procedure position_cursor is  -- GUI related
			use et_geometry;
			coordinates : type_coordinates := to_coordinates (f (5));
			position : type_point := type_point (set (
					x => to_distance (f (6)),
					y => to_distance (f (7))));
		begin
			log (text => "place cursor" & to_string (coordinates) 
				& to_string (position), level => log_threshold + 1);
			
			canvas.move_cursor (coordinates, cursor_main, position);
		end position_cursor;		

		-- CS unify procedures show_unit and show_first_unit. They differ only in 
		-- the way the unit_name is assigned.
		
		-- Locates the unit of the given device.
		procedure show_unit is -- GUI related
			use schematic_ops;
			use et_devices;
			use et_canvas_schematic;
			
			device_name : et_devices.type_name := to_name (f (5)); -- IC45
			unit_name	: et_devices.type_unit_name.bounded_string := to_name (f (6)); -- C
			
			-- Locate the requested device and unit.
			location : type_unit_query := unit_position (
					module_cursor	=> current_active_module,
					device_name		=> device_name,
					unit_name		=> unit_name);
		begin
			-- CS log message ?
			
			if location.exists then
				-- show the sheet where the unit is:
				current_active_sheet := sheet (location.position);

				-- center on the unit
				center_on (canvas, type_point (location.position));
			end if;
			
			log (text => to_string (device_name, unit_name, location), console => true);
		end show_unit;

		-- Locates the one and only unit of the given device.
		procedure show_first_unit is -- GUI related
			use schematic_ops;
			use et_devices;
			use et_canvas_schematic;
			
			device_name : et_devices.type_name := to_name (f (5)); -- IC45

			-- The assumption is that the device has only one unit:
			unit_name	: et_devices.type_unit_name.bounded_string := to_name ("");
			
			-- Locate the requested device and unit.
			location : type_unit_query := unit_position (
					module_cursor	=> current_active_module,
					device_name		=> device_name,
					unit_name		=> unit_name);
		begin
			-- CS log message ?
			
			if location.exists then
				-- show the sheet where the unit is:
				current_active_sheet := sheet (location.position);

				-- center on the unit
				center_on (canvas, type_point (location.position));
			end if;

			log (text => to_string (device_name, unit_name, location), console => true);
		end show_first_unit;

		procedure show_sheet is -- GUI related
			use et_canvas_schematic;
			sheet : et_coordinates.type_sheet := to_sheet (f (5));
		begin
			log (text => "set sheet" & to_sheet (sheet), level => log_threshold + 1); 

			-- CS test if sheet exists
			
			current_active_sheet := sheet;

			-- Update module name and sheet number in the schematic window title bar:
			set_title_bar (active_module, current_active_sheet);
		end show_sheet;

		-- Sets the active module and first sheet.
		procedure show_module is -- GUI related
			use et_general;
			use et_project;
			use et_canvas_schematic;
			
			module : type_module_name.bounded_string := to_module_name (f (5));
		begin
			log (text => "set module " & enclose_in_quotes (to_string (module)), level => log_threshold + 1);
			set_module (module);
			current_active_sheet := 1;

			-- Update module name and sheet number in the schematic window title bar:
			set_title_bar (module, current_active_sheet);
			
			-- Update the board window title bar:
			et_canvas_board.set_title_bar (module);
		end show_module;

		procedure show_module_and_sheet is  -- GUI related
		-- Sets the active module and sheet.
			use et_general;
			use et_canvas_schematic;
			
			module : type_module_name.bounded_string := to_module_name (f (5));
			sheet : et_coordinates.type_sheet := to_sheet (f (6));
		begin
			log (text => "set module " & enclose_in_quotes (to_string (module))
				& " sheet " & to_sheet (sheet), level => log_threshold + 1);
			set_module (module);
			current_active_sheet := sheet;

			-- Update module name and sheet number in the schematic window title bar:
			set_title_bar (module, current_active_sheet);
			
			-- Update the board window title bar:
			et_canvas_board.set_title_bar (module);
		end show_module_and_sheet;

		-- Enables a certain layer. If status is empty, the layer will be enabled.
		procedure display ( -- GUI related
			layer	: in type_noun_schematic;
			status	: in string := "") is

			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;
			
			log (text => "display " & to_lower (to_string (layer)) 
				 & space & to_string (ls),
				 level => log_threshold + 1);
			
			case layer is
				when NOUN_GRID		=> layers.grid := ls;
				when NOUN_NAMES		=> layers.device_names := ls;
				when NOUN_NETS		=> layers.nets := ls;
				when NOUN_PORTS		=> layers.ports := ls;
				when NOUN_PURPOSES	=> layers.device_purposes := ls;
				when NOUN_TEXTS		=> layers.texts := ls;
				when NOUN_VALUES	=> layers.device_values := ls;
				
				when others => 
					log (importance => ERROR, text => "invalid layer !", console => true);
			end case;

			-- CS exception handler if status is invalid
		end display;
		
	begin -- schematic_cmd
		log (text => "full command: " & enclose_in_quotes (to_string (cmd)), level => log_threshold);

		domain := to_domain (f (1)); -- DOM_SCHEMATIC
		module := to_module_name (f (2)); -- motor_driver (without extension *.mod)

		-- read the verb from field 3
		verb := to_verb (f (3));

		-- There are some very short commands which do not require a verb.
		-- For such commands we do not read the noun.
		case verb is
			when VERB_EXIT | VERB_QUIT => null; -- no noun
			when others => noun := to_noun (f (4)); -- read noun from field 4
		end case;

		-- parse the command:	
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 9 =>
								-- If a virtual device is added, then no variant is required.
								schematic_ops.add_device (
									module_name 	=> module,
									device_model	=> to_file_name (f (5)),
									place			=> to_position 
										(
										sheet => to_sheet (f (6)),
										point => type_point (set 
													(
													x => to_distance (f (7)),
													y => to_distance (f (8))
													)),
										rotation => to_rotation (f (9))
										),
									variant			=> to_name (""),
									log_threshold	=> log_threshold + 1
									);

							when 10 =>
								-- A real device requires specification of a package variant.
								schematic_ops.add_device (
									module_name 	=> module,
									device_model	=> to_file_name (f (5)),
									place			=> to_position 
										(
										sheet => to_sheet (f (6)),
										point => type_point (set 
													(
													x => to_distance (f (7)),
													y => to_distance (f (8))
													)),
										rotation		=> to_rotation (f (9))
										),
									variant			=> to_name (f (10)),
									log_threshold	=> log_threshold + 1
									);

							when 11 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_NETCHANGER =>
						case fields is
							when 8 =>
								schematic_ops.add_netchanger (
									module_name 	=> module,
									place			=> to_position 
										(
										sheet => to_sheet (f (5)),
										point => type_point (set 
													(
													x => to_distance (f (6)),
													y => to_distance (f (7))
													)),
										rotation		=> to_rotation (f (8))
										),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_PORT =>
						case fields is
							when 9 =>
								schematic_ops.add_port (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									port_name		=> et_general.to_net_name (f (6)),
									position		=> type_point (set 
												(
												x => to_distance (f (7)),
												y => to_distance (f (8))
												)),
									direction		=> submodules.to_port_name (f (9)),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SUBMODULE =>
						case fields is
							when 11 =>
								schematic_ops.add_submodule (
									module_name 	=> module, -- parent module (where the submodule is to be inserted)
									file			=> submodules.to_submodule_path (f (5)),
									instance		=> et_general.to_instance_name (f (6)), -- submodule instance name
									position		=> to_position 
										(
										sheet => to_sheet (f (7)),
										point => type_point (set 
													(
													x => to_distance (f (8)),
													y => to_distance (f (9))
													))
										),
									size => (
										x => to_distance (f (10)),
										y => to_distance (f (11))
										),
									log_threshold	=> log_threshold + 1
									);

							when 12 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_BUILD =>
				case noun is
					when NOUN_SUBMODULES_TREE =>
						case fields is
							when 4 =>
								schematic_ops.build_submodules_tree (
									module_name 	=> module,
									log_threshold	=> log_threshold + 1
									);

							when 5 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_CHECK =>
				case noun is
					when NOUN_INTEGRITY =>
						case fields is
							when 4 =>
								schematic_ops.check_integrity (
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
							
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_COPY =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 9 =>
								schematic_ops.copy_device (
									module_name 	=> module,
									device_name		=> to_name (f (5)),
									destination		=> to_position 
										(
										sheet => to_sheet (f (6)),
										point => type_point (set
													(
													x => to_distance (f (7)),
													y => to_distance (f (8))
													)),
										rotation		=> to_rotation (f (9))
										),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_SUBMODULE =>
						case fields is
							when 9 =>
								schematic_ops.copy_submodule (
									module_name 	=> module, -- parent module (where the submodule is to be copied)
									instance_origin	=> et_general.to_instance_name (f (5)), -- submodule instance name
									instance_new	=> et_general.to_instance_name (f (6)), -- submodule instance name
									destination		=> to_position 
										(
										sheet => to_sheet (f (7)),
										point => type_point (set
													(
													x => to_distance (f (8)),
													y => to_distance (f (9))
													))
										),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_CREATE =>
				case noun is
					when NOUN_VARIANT => 
						case fields is
							when 5 =>
								schematic_ops.create_assembly_variant
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)),
									log_threshold	=> log_threshold + 1);
								
							when 6 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
																	
			when VERB_DELETE =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 5 =>
								schematic_ops.delete_device (
									module_name 	=> module,
									device_name		=> to_name (f (5)),
									log_threshold	=> log_threshold + 1);

							when 6 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_LABEL =>
						case fields is
							when 7 =>
								schematic_ops.delete_net_label
									(
									module_name		=> module,

									position		=> to_position (
														point => type_point (set (
															x => to_distance (f (6)),
															y => to_distance (f (7)))),
														sheet => to_sheet (f (5))), -- sheet number
									
									log_threshold	=> log_threshold + 1);
								
							when 8 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_NET =>
						case fields is

							-- If the statement has only 6 fields, the net scope is EVERYWHERE.
							-- Place assumes default (sheet 1, x/y 0/0) and is further-on ignored 
							-- by the called procedure:
							when 5 =>
								schematic_ops.delete_net
									(
									module_name			=> module,
									net_name			=> to_net_name (f (5)), -- RESET
									scope				=> EVERYWHERE,
									place				=> to_position (
															point => origin,
															sheet => 1),
									log_threshold		=> log_threshold + 1);

							-- If the statement has 7 fields, the net scope is SHEET.
							-- Sheet is set by the 7th argument. x and y assume default (0/0)
							-- and are further-on ignored by the called procedure:
							when 6 =>
								schematic_ops.delete_net
									(
									module_name			=> module,
									net_name			=> to_net_name (f (5)), -- RESET
									scope				=> SHEET,
									place				=> to_position (
															point => origin,
															sheet => to_sheet (f (6))), -- sheet number
									log_threshold		=> log_threshold + 1);

							-- If the statement has 9 fields, the net scope is STRAND.
							-- Place is set according to arguments 7..9.
							when 8 =>
								schematic_ops.delete_net
									(
									module_name			=> module,
									net_name			=> to_net_name (f (5)), -- RESET
									scope				=> STRAND,
									place				=> to_position (
															point => type_point (set (
																x => to_distance (f (7)),
																y => to_distance (f (8)))),
															sheet => to_sheet (f (6))), -- sheet number
									log_threshold		=> log_threshold + 1);

								
							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);

						end case;

					when NOUN_NETCHANGER =>
						case fields is
							when 5 =>
								schematic_ops.delete_netchanger
									(
									module_name		=> module,
									index			=> submodules.to_netchanger_id (f (5)), -- 1,2,3,...
									log_threshold		=> log_threshold + 1);

							when 6 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_PORT =>
						case fields is
							when 6 =>
								schematic_ops.delete_port
									(
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									port_name		=> et_general.to_net_name (f (6)),
									log_threshold	=> log_threshold + 1
									);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SEGMENT =>
						case fields is
							when 8 =>
								schematic_ops.delete_segment
									(
									module_name		=> module,
									net_name		=> to_net_name (f (5)), -- RESET
									place			=> to_position (
														point => type_point (set (
															x => to_distance (f (7)),
															y => to_distance (f (8)))),
														sheet => to_sheet (f (6))), -- sheet number
									log_threshold	=> log_threshold + 1);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_SUBMODULE =>
						case fields is
							when 5 =>
								schematic_ops.delete_submodule (
									module_name 	=> module, -- parent module (where the submodule is to be deleted)
									instance		=> et_general.to_instance_name (f (5)), -- submodule instance name
									log_threshold	=> log_threshold + 1
									);

							when 6 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_TEXT =>
						NULL; -- CS
						
					when NOUN_UNIT =>
						case fields is
							when 6 =>
								schematic_ops.delete_unit (
									module_name 	=> module,
									device_name		=> to_name (f (5)),
									unit_name		=> to_name (f (6)),
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_VARIANT => 
						case fields is
							when 5 =>
								schematic_ops.delete_assembly_variant
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)),
									log_threshold	=> log_threshold + 1);
								
							when 6 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_DESCRIBE =>
				case noun is
					when NOUN_VARIANT => 
						case fields is
							when 6 =>
								schematic_ops.describe_assembly_variant
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									description		=> assembly_variants.to_unbounded_string (f (6)), -- "the cheap version"
									log_threshold	=> log_threshold + 1);
								
							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_DISPLAY => -- GUI related
				case noun is
					when NOUN_PORTS		-- like "schematic led_driver display ports [on/off]"
						| NOUN_NETS		-- like "schematic led_driver display nets [on/off]"
						| NOUN_NAMES | NOUN_VALUES | NOUN_PURPOSES
						| NOUN_TEXTS | NOUN_GRID
						=>
						case fields is
							when 4 => display (noun); -- if status is omitted
							when 5 => display (noun, f (5));
							when 6 .. count_type'last => too_long; 
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
						
			when VERB_DRAG =>
				case noun is
					when NOUN_UNIT =>
						case fields is
							when 9 =>
								schematic_ops.drag_unit
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)),
									unit_name		=> to_name (f (6)),
									coordinates		=> to_coordinates (f (7)), -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_NETCHANGER =>
						case fields is
							when 8 =>
								schematic_ops.drag_netchanger (
									module_name 	=> module,
									index			=> submodules.to_netchanger_id (f (5)), -- 1,2,3,...
									coordinates		=> to_coordinates (f (6)), -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_PORT =>
						case fields is
							when 9 =>
								schematic_ops.drag_port (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									port_name		=> et_general.to_net_name (f (6)),
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SEGMENT =>
						case fields is
							when 11 =>
								schematic_ops.drag_segment
									(
									module_name		=> module,
									net_name		=> to_net_name (f (5)), -- RESET
									place			=> to_position (
														point => type_point (set (
															x => to_distance (f (7)),
															y => to_distance (f (8)))),
														sheet => to_sheet (f (6))), -- sheet number
									
									coordinates		=> to_coordinates (f (9)), -- relative/absolute
									
									point			=> type_point (set (
														x => to_distance (f (10)),
														y => to_distance (f (11)))),
									
									log_threshold	=> log_threshold + 1);

							when 12 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SUBMODULE =>
						case fields is
							when 8 =>
								schematic_ops.drag_submodule (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									point			=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						case fields is
							when 10 =>
								schematic_ops.draw_net
									(
									module_name		=> module,
									net_name		=> to_net_name (f (5)), -- RESET
									start_point		=> to_position (
															point => type_point (set (
																x => to_distance (f (7)),
																y => to_distance (f (8)))),
															sheet => to_sheet (f (6))), -- sheet number
									
									end_point		=> type_point (set (
														x => to_distance (f (9)),
														y => to_distance (f (10)))),
									
									log_threshold	=> log_threshold + 1);

							when 11 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_EXECUTE =>
				case noun is
					when NOUN_SCRIPT =>
						case fields is
							when 5 => 
								execute_nested_script (
									file			=> f (5),
									log_threshold	=> log_threshold + 1);

							when 6 .. count_type'last => too_long;								
							when others => command_incomplete (cmd);
						end case;
							
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_EXIT | VERB_QUIT => terminate_main;
				
			when VERB_INVOKE =>
				case noun is
					when NOUN_UNIT =>
						case fields is
							when 10 =>
								schematic_ops.invoke_unit (
									module_name		=> module,
									device_name		=> to_name (f (5)),
									unit_name		=> to_name (f (6)),
									place			=> to_position 
										(
										sheet => to_sheet (f (7)),
										point => type_point (set
													(
													x => to_distance (f (8)),
													y => to_distance (f (9))
													)),
										rotation		=> to_rotation (f (10))
										),
									log_threshold	=> log_threshold + 1
									);

							when 11 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_MOVE =>
				case noun is
					when NOUN_NAME =>
						case fields is
							when 9 =>
								schematic_ops.move_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									meaning			=> et_symbols.NAME,
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_VALUE =>
						case fields is
							when 9 =>
								schematic_ops.move_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									meaning			=> et_symbols.VALUE,
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_PORT =>
						case fields is
							when 9 =>
								schematic_ops.move_port (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									port_name		=> et_general.to_net_name (f (6)),
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_PURPOSE =>
						case fields is
							when 9 =>
								schematic_ops.move_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									meaning			=> et_symbols.PURPOSE,
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_NETCHANGER =>
						case fields is
							when 9 =>
								schematic_ops.move_netchanger
									(
									module_name 	=> module,
									index			=> submodules.to_netchanger_id (f (5)), -- 1,2,3, ...
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									sheet			=> to_sheet_relative (f (7)),
									point			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
										
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_TEXT =>
						NULL; -- CS

					when NOUN_SUBMODULE =>
						case fields is
							when 9 =>
								schematic_ops.move_submodule (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									sheet			=> to_sheet_relative (f (7)),
									point			=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_UNIT =>
						case fields is
							when 10 =>
								schematic_ops.move_unit
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									sheet			=> to_sheet_relative (f (8)),
									point			=> type_point (set (
														x => to_distance (f (9)),
														y => to_distance (f (10)))),
										
									log_threshold	=> log_threshold + 1
									);

							when 11 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_MAKE =>
				case noun is
					when NOUN_BOM => 
						case fields is
							when 4 =>
								schematic_ops.make_boms -- a BOM for each variant
									(
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_NETLISTS => 
						case fields is
							when 4 =>
								schematic_ops.make_netlists 
									(
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_MOUNT =>
				case noun is
					when NOUN_DEVICE => 
						declare
							value : type_value.bounded_string; -- 470R
							partcode : material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
							purpose : type_purpose.bounded_string; -- brightness_control
						begin
							-- validate value
							value := to_value (f (7));

							-- validate partcode
							partcode := material.to_partcode (f (8));
							
							case fields is
								when 8 =>
									-- set value and partcode
									schematic_ops.mount_device
										(
										module_name		=> module,
										variant_name	=> to_variant (f (5)), -- low_cost
										device			=> to_name (f (6)), -- R1
										value			=> value, -- 220R
										partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
										log_threshold	=> log_threshold + 1);

								when 9 =>
									-- optionally the purpose can be set also
									purpose := to_purpose (f (9)); -- brightness_control
												
									schematic_ops.mount_device
										(
										module_name		=> module,
										variant_name	=> to_variant (f (5)), -- low_cost
										device			=> to_name (f (6)), -- R1
										value			=> value, -- 220R
										partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
										purpose			=> purpose, -- brightness_control
										log_threshold	=> log_threshold + 1);
									
								when 10 .. count_type'last => command_too_long (cmd, fields - 1);
									
								when others => command_incomplete (cmd);
							end case;

						end; -- declare

					when NOUN_SUBMODULE =>
						case fields is
							when 7 =>
								schematic_ops.mount_submodule
									(
									module_name		=> module,
									variant_parent	=> to_variant (f (5)), -- low_cost
									instance		=> et_general.to_instance_name (f (6)), -- OSC1
									variant_submod	=> to_variant (f (7)), -- fixed_frequency
									log_threshold	=> log_threshold + 1);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);

						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_PLACE =>
				case noun is
					when NOUN_JUNCTION =>
						case fields is
							when 7 =>
								schematic_ops.place_junction 
									(
									module_name 	=> module,
									place			=> to_position 
														(
														sheet => to_sheet (f (5)),
														point => type_point (set (
																	x => to_distance (f (6)),
																	y => to_distance (f (7))
																	))
														),
										
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_LABEL =>
						case fields is
							when 10 =>
								-- SIMPLE LABEL
								schematic_ops.place_net_label
									(
									module_name			=> module,

									segment_position	=> to_position (
															point => type_point (set (
																x => to_distance (f (6)),
																y => to_distance (f (7)))),
															sheet => to_sheet (f (5))), -- sheet number

									label_position		=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),

									rotation			=> et_coordinates.geometry.to_rotation (f (10)), -- 0 / 90
									appearance 			=> et_schematic.SIMPLE,

									-- A simple label does not indicate the direction
									-- of information flow. But this procedure call requires a
									-- direction. So we just pass direction PASSIVE. It has no 
									-- further meaning.
									direction			=> et_schematic.PASSIVE,

									log_threshold		=> log_threshold + 1);

							when 8 =>
								-- TAG LABEL
								schematic_ops.place_net_label
									(
									module_name			=> module,

									segment_position	=> to_position (
															point => type_point (set (
																x => to_distance (f (6)),
																y => to_distance (f (7)))),
															sheet => to_sheet (f (5))), -- sheet number

									appearance 			=> et_schematic.TAG,

									-- A tag label requires specification of signal direction:
									direction			=> et_schematic.to_direction (f (8)), -- INPUT, OUTPUT, PASSIVE, ...

									log_threshold		=> log_threshold + 1);
								
							when 11 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd); -- incl. field count of 9
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_POSITION => -- GUI related
				case noun is 
					when NOUN_CURSOR =>
						case fields is
							when 7 => position_cursor; -- position cursor absolute/relative 25 30
							when 8 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_REMOVE =>
				case noun is
					when NOUN_DEVICE => 
						case fields is
							when 6 =>
								schematic_ops.remove_device -- from assembly variant
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									device			=> to_name (f (6)), -- R1
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_SUBMODULE =>
						case fields is
							when 6 =>
								schematic_ops.remove_submodule
									(
									module_name		=> module,
									variant_parent	=> to_variant (f (5)),
									instance		=> et_general.to_instance_name (f (6)), -- OSC1
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 6 =>
								schematic_ops.rename_device
									(
									module_name 		=> module,
									device_name_before	=> to_name (f (5)), -- IC1
									device_name_after	=> to_name (f (6)), -- IC23
									log_threshold		=> log_threshold + 1
									);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case; 
								
					when NOUN_SUBMODULE =>
						case fields is
							when 6 =>
								schematic_ops.rename_submodule
									(
									module_name		=> module,
									instance_old	=> et_general.to_instance_name (f (5)), -- OSC1
									instance_new	=> et_general.to_instance_name (f (6)), -- OSC2
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_NET =>
						case fields is

							-- If the statement has only 6 fields, the net scope is EVERYWHERE.
							-- Place assumes default (sheet 1, x/y 0/0) and is further-on ignored 
							-- by the called procedure:
							when 6 =>
								schematic_ops.rename_net
									(
									module_name			=> module,
									net_name_before		=> to_net_name (f (5)), -- RESET
									net_name_after		=> to_net_name (f (6)), -- RESET_N
									scope				=> EVERYWHERE,
									place				=> to_position (
															point => origin,
															sheet => 1),
									log_threshold		=> log_threshold + 1);

							-- If the statement has 7 fields, the net scope is SHEET.
							-- Sheet is set by the 7th argument. x and y assume default (0/0)
							-- and are further-on ignored by the called procedure:
							when 7 =>
								schematic_ops.rename_net
									(
									module_name			=> module,
									net_name_before		=> to_net_name (f (5)), -- RESET
									net_name_after		=> to_net_name (f (6)), -- RESET_N
									scope				=> SHEET,
									place				=> to_position (
															point => origin,
															sheet => to_sheet (f (7))), -- sheet number
									log_threshold		=> log_threshold + 1);

							-- If the statement has 9 fields, the net scope is STRAND.
							-- Place is set according to arguments 7..9.
							when 9 =>
								schematic_ops.rename_net
									(
									module_name			=> module,
									net_name_before		=> to_net_name (f (5)), -- RESET
									net_name_after		=> to_net_name (f (6)), -- RESET_N
									scope				=> STRAND,
									place				=> to_position (
															point => type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
															sheet => to_sheet (f (7))), -- sheet number
									log_threshold		=> log_threshold + 1);

								
							when 10 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_RENUMBER =>
				case noun is
					when NOUN_DEVICES =>
						case fields is
							when 5 =>
								schematic_ops.renumber_devices
									(
									module_name 	=> module,
									step_width		=> to_index (f (5)), -- 100
									log_threshold	=> log_threshold + 1
									);

							when 6 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_ROTATE =>
				case noun is
					when NOUN_TEXT =>
						NULL; -- CS

					when NOUN_UNIT =>
						case fields is
							when 8 =>
								schematic_ops.rotate_unit
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									rotation		=> to_rotation (f (8)), -- 90
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_NAME =>
						case fields is 
							when 7 =>
								schematic_ops.rotate_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									rotation		=> et_schematic.pac_text.to_rotation_doc (f (7)), -- 90
									meaning			=> et_symbols.NAME,
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_VALUE =>
						case fields is
							when 7 =>
								schematic_ops.rotate_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									rotation		=> et_schematic.pac_text.to_rotation_doc (f (7)), -- 90
									meaning			=> et_symbols.VALUE,
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_PURPOSE =>
						case fields is
							when 7 =>
								schematic_ops.rotate_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									unit_name		=> to_name (f (6)), -- A
									rotation		=> et_schematic.pac_text.to_rotation_doc (f (7)), -- 90
									meaning			=> et_symbols.PURPOSE,
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_NETCHANGER =>
						case fields is
							when 7 =>
								schematic_ops.rotate_netchanger (
									module_name 	=> module,
									index			=> submodules.to_netchanger_id (f (5)), -- 1,2,3,...
									coordinates		=> to_coordinates (f (6)), -- relative/absolute
									rotation		=> to_rotation (f (7)), -- 90
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_SET =>
				case noun is
					when NOUN_GRID =>
						case fields is
							-- schematic led_driver set grid 5 5
							when 6 =>
								schematic_ops.set_grid (
									module_name 	=> module,
									grid			=> (
											x => to_distance (f (5)),
											y => to_distance (f (6))),
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
				
					when NOUN_PARTCODE =>
						case fields is
							when 6 =>
								declare
									partcode : material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
								begin
									partcode := material.to_partcode (f (6));

									-- set the purpose
									schematic_ops.set_partcode
										(
										module_name 	=> module,
										device_name		=> to_name (f (5)), -- R1
										partcode		=> partcode, -- R_PAC_S_0805_VAL_100R
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_PURPOSE =>
						case fields is
							when 6 =>
								declare
									use et_schematic;
									purpose : type_purpose.bounded_string; -- brightness_control
								begin
									purpose := to_purpose (f (6));
									
									-- set the purpose
									schematic_ops.set_purpose
										(
										module_name 	=> module,
										device_name		=> to_name (f (5)), -- R1
										purpose			=> purpose, -- brightness_control
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SCOPE =>
						case fields is
							when 6 =>
								schematic_ops.set_scope (
									module_name 	=> module,
									net_name		=> et_general.to_net_name (f (5)),
									scope			=> netlists.to_net_scope (f (6)),
									log_threshold	=> log_threshold + 1
									);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SUBMODULE_FILE =>
						case fields is
							when 6 =>
								schematic_ops.set_submodule_file (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)),
									file			=> submodules.to_submodule_path (f (6)),
									log_threshold	=> log_threshold + 1
									);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_VALUE =>
						case fields is
							when 6 =>
								declare
									value : type_value.bounded_string; -- 470R
								begin
									-- validate value
									value := to_value (f (6));

									-- set the value
									schematic_ops.set_value
										(
										module_name 	=> module,
										device_name		=> to_name (f (5)), -- R1
										value			=> value, -- 470R
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. count_type'last => command_too_long (cmd, fields - 1); 
								
							when others => command_incomplete (cmd);
						end case;
								
					when NOUN_TEXT_SIZE =>
						NULL; -- CS
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_SHOW => -- GUI related
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 5 => show_first_unit; -- show device R1
							when 6 => show_unit; -- show device IC45 C
							when 7 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when NOUN_MODULE =>
						case fields is
							when 5 => show_module; -- show module LED-driver
							when 6 => show_module_and_sheet; -- show module LED-driver 2
							when 7 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SHEET =>
						case fields is
							when 5 => show_sheet;
							when 6 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_UNMOUNT =>
				case noun is
					when NOUN_DEVICE => 
						case fields is
							when 6 =>
								schematic_ops.unmount_device
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									device			=> to_name (f (6)), -- R1
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_WRITE =>
				case noun is
					when NOUN_TEXT =>
						NULL; -- CS

					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_ZOOM => -- GUI related
				case noun is
					when NOUN_FIT => -- zoom fit
						case fields is
							when 4 => 
								log (text => "zoom to fit", level => log_threshold + 1);
								scale_to_fit (canvas);

							when 5 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;

					when NOUN_LEVEL => -- zoom level 3
						case fields is
							when 5 => 
								set_scale (f (5));

							when 6 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_CENTER => -- zoom center 10 10
						case fields is
							when 6 =>  -- zoom center 10 10
								zoom_center;

							when 7 =>  -- zoom center 10 10 0.5
								zoom_center;
								set_scale (f (7));

							when 8 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
		end case;
		
		return exit_code;
		
		exception when event: others => 
		
			log (ERROR, "schematic command " & enclose_in_quotes (to_string (cmd)) &
				" invalid !", console => true);

			log (text => ada.exceptions.exception_information (event), console => true);		

		return ERROR;

	end schematic_cmd;

	function board_cmd (
		cmd				: in type_fields_of_line; -- "board tree_1 draw silk top line 2.5 0 0 160 0"
		log_threshold	: in type_log_level)
		return type_exit_code is

		use et_packages;
		use et_packages.pac_shapes;
		use et_pcb;
		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
		use et_pcb_stack;
		use et_canvas_board.pac_canvas;
		use et_display.board;

		-- The exit code will be overridden with ERROR or WARNING if something goes wrong:
		exit_code : type_exit_code := SUCCESSFUL;
		
		domain	: type_domain; -- DOM_BOARD
		module	: type_module_name.bounded_string; -- motor_driver (without extension *.mod)

		verb	: type_verb_board;
		noun	: type_noun_board;
		
		
		function f (place : in positive) return string is begin
			return et_string_processing.field (cmd, place);
		end;

		function fields return count_type is begin
			return et_string_processing.field_count (cmd);
		end;

		procedure too_long is begin -- CS use it more often
			command_too_long (cmd, fields - 1);
		end;

		-- Enables/disables the grid "layer". If status is empty,
		-- the layer will be enabled.
		procedure display_grid ( -- GUI related
			status	: in string := "") is 

			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			log (text => "display grid layer" & space & to_string (ls),
				 level => log_threshold + 1);

			layers.grid := ls;
			
			-- CS exception handler if status is invalid
		end display_grid;
		
		-- Enables/disables the outline layer. If status is empty,
		-- the layer will be enabled.
		procedure display_outline ( -- GUI related
			status	: in string := "") is 

			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			log (text => "display outline layer" & space & to_string (ls),
				 level => log_threshold + 1);

			layers.outline := ls;
			
			-- CS exception handler if status is invalid
		end display_outline;
		
		-- Enables/disables a certain non-conductor layer. If status is empty,
		-- the layer will be enabled.
		procedure display_non_conductor_layer ( -- GUI related
			layer	: in type_noun_board;
			face	: in string; -- top/bottom
			status	: in string := "") is 

			ls : type_layer_status;
			fc : type_face;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given face to type_face:
			fc := to_face (face);
			
			log (text => "display " & to_lower (to_string (layer)) 
				 & space & to_string (ls),
				 level => log_threshold + 1);

			case fc is
				when TOP =>
					case layer is
						when NOUN_SILKSCREEN 	=> layers.silkscreen.top	:= ls;
						when NOUN_ASSY			=> layers.assy_doc.top		:= ls;
						when NOUN_KEEPOUT		=> layers.keepout.top		:= ls;
						when NOUN_STENCIL		=> layers.stencil.top		:= ls;
						when NOUN_STOP			=> layers.stop_mask.top		:= ls;
						
						when others => 
							log (importance => ERROR, text => "invalid layer !", console => true);
					end case;

				when BOTTOM =>
					case layer is
						when NOUN_SILKSCREEN 	=> layers.silkscreen.bottom	:= ls;
						when NOUN_ASSY			=> layers.assy_doc.bottom	:= ls;
						when NOUN_KEEPOUT		=> layers.keepout.bottom	:= ls;
						when NOUN_STENCIL		=> layers.stencil.bottom	:= ls;
						when NOUN_STOP			=> layers.stop_mask.bottom	:= ls;
						
						when others => 
							log (importance => ERROR, text => "invalid layer !", console => true);
					end case;
			end case;
			
			-- CS exception handler if status is invalid
		end display_non_conductor_layer;

		-- Enables/disables a certain conductor layer. 
		-- If status is empty, the layer will be enabled.
		procedure display_conductor_layer ( -- GUI related
			layer	: in string;
			status	: in string := "") is 

			ls : type_layer_status;
			ly : type_signal_layer;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given layer to type_signal_layer:
			ly := to_signal_layer (layer);
			
			log (text => "display conductor layer " & to_string (ly) & space & to_string (ls),
				 level => log_threshold + 1);

			layers.conductors (ly) := ls;
			
			-- CS exception handler if status is invalid
		end display_conductor_layer;

		-- Enables/disables a the via layer. 
		-- If status is empty, the layer will be enabled.
		procedure display_vias ( -- GUI related
			--layer	: in string;
			status	: in string := "") is 

			ls : type_layer_status;
			--ly : type_signal_layer;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given layer to type_signal_layer:
			--ly := to_signal_layer (layer);
			
			--log (text => "display via layer " & to_string (ly) & space & to_string (ls),
			log (text => "display via layer " & space & to_string (ls),
				 level => log_threshold + 1);

			--layers.vias (ly) := ls;
			layers.vias := ls;
			
			-- CS exception handler if status is invalid
		end display_vias;
		
		-- Enables/disables a certain restrict layer. 
		-- If status is empty, the layer will be enabled.
		procedure display_restrict_layer ( -- GUI related
			objects	: in string; -- route/via
			layer	: in string; -- 1, 2, 8, ...
			status	: in string := "") is 

			ls : type_layer_status;
			ly : type_signal_layer;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given layer to type_signal_layer:
			ly := to_signal_layer (layer);
			
			if objects = keyword_route then
				log (text => "display route restrict layer " & to_string (ly) & space & to_string (ls),
					level => log_threshold + 1);

				layers.route_restrict (ly) := ls;
				
			elsif objects = keyword_via then
				log (text => "display via restrict layer " & to_string (ly) & space & to_string (ls),
					level => log_threshold + 1);

				layers.via_restrict (ly) := ls;
				
			else
				log (importance => ERROR, 
					 text => "Expect keyword " &
						enclose_in_quotes (keyword_route) & " or " &
						enclose_in_quotes (keyword_via) & "after noun " &
						to_string (NOUN_RESTRICT) & " !",
					 console => true);
				raise constraint_error;
			end if;
			
			-- CS exception handler if status is invalid
		end display_restrict_layer;
		

		procedure draw_keepout is
			shape : type_shape := to_shape (f (6));
		begin
			case shape is
				when LINE =>
					case fields is
						when 10 =>
							board_ops.draw_keepout_line (
								module_name 	=> module,
								face			=> to_face (f (5)),
								line			=> (
											start_point	=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											end_point	=> type_point (set (
												x => to_distance (f (9)),
												y => to_distance (f (10))))
											),

								log_threshold	=> log_threshold + 1);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when ARC =>
					case fields is
						when 13 =>
							board_ops.draw_keepout_arc (
								module_name 	=> module,
								face			=> to_face (f (5)),
								arc				=> (
											center	=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											start_point	=> type_point (set (
												x => to_distance (f (9)),
												y => to_distance (f (10)))),
											end_point	=> type_point (set (
												x => to_distance (f (11)),
												y => to_distance (f (12)))),
											direction	=> to_direction (f (13))
											),

								log_threshold	=> log_threshold + 1);

						when 14 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when CIRCLE =>
					case fields is
						when 9 =>
						-- board led_driver draw keepout top circle 50 50 40 -- 9 fields
							
							if is_number (f (7)) then
								-- Circle is not filled.
								
								board_ops.draw_keepout_circle (
									module_name 	=> module,
									face			=> to_face (f (5)),
									circle			=> 
												(
												filled		=> NO,
												center	=> type_point (set (
															x => to_distance (f (7)),
															y => to_distance (f (8)))),
												radius	=> to_distance (f (9))
												),
									log_threshold	=> log_threshold + 1);
							else
								expect_value_center_x (7);
							end if;

						when 10 =>
						-- board led_driver draw keepout top circle filled 50 50 40 -- 10 fields
							
							if f (7) = keyword_filled then
								-- Circle is filled.
								
								board_ops.draw_keepout_circle (
									module_name 	=> module,
									face			=> to_face (f (5)),
									circle			=> 
												(
												filled		=> YES,
												center	=> type_point (set (
															x => to_distance (f (8)),
															y => to_distance (f (9)))),
												radius	=> to_distance (f (10))
												),
									log_threshold	=> log_threshold + 1);
							else
								expect_keyword_filled (7);
							end if;
								
						when 12 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

							
				when others => null;
			end case;
		end draw_keepout;
		
		procedure draw_route_restrict is
			shape : type_shape := to_shape (f (6));
		begin
			case shape is
				when LINE =>
					case fields is
						when 10 =>
							-- board led_driver draw route_restrict [1,3,5-9] line 10 10 60 10
							board_ops.draw_route_restrict_line (
								module_name 	=> module,
								line			=> (
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											start_point	=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											end_point	=> type_point (set (
												x => to_distance (f (9)),
												y => to_distance (f (10))))
											),

								log_threshold	=> log_threshold + 1);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when ARC =>
					case fields is
						when 13 =>
							-- board led_driver draw route_restrict [1,3,5-9] arc 50 50 0 50 100 0 cw
							board_ops.draw_route_restrict_arc (
								module_name 	=> module,
								arc				=> (
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											center	=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											start_point	=> type_point (set (
												x => to_distance (f (9)),
												y => to_distance (f (10)))),
											end_point	=> type_point (set (
												x => to_distance (f (11)),
												y => to_distance (f (12)))),
											direction	=> to_direction (f (13))
											),

								log_threshold	=> log_threshold + 1);

						when 14 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when CIRCLE =>
					case fields is
						when 9 =>
							-- board led_driver draw route_restrict [1,3,5-9] circle 20 50 40
							if is_number (f (7)) then -- 20

								-- Circle is not filled.
								board_ops.draw_route_restrict_circle (
									module_name 	=> module,
									circle			=> 
												(
												layers		=> to_layers (f (5)), -- [1,3,5-9]
												filled		=> NO,
												center	=> type_point (set (
															x => to_distance (f (7)), -- 20
															y => to_distance (f (8)))), -- 50
												radius	=> to_distance (f (9)) -- 40
												),
												
									log_threshold	=> log_threshold + 1);
							else
								expect_value_center_x (7);
							end if;

						when 10 =>
							-- Circle is filled.
							-- board led_driver draw route_restrict [1,3,5-9] circle filled 20 50 40
							if f (7) = keyword_filled then

								-- Circle is filled.
								board_ops.draw_route_restrict_circle (
									module_name 	=> module,
									circle			=> 
												(
												layers		=> to_layers (f (5)), -- [1,3,5-9]
												filled		=> YES,
												center	=> type_point (set (
															x => to_distance (f (8)), -- 20
															y => to_distance (f (9)))), -- 50
												radius	=> to_distance (f (10)) -- 40
												),
												
									log_threshold	=> log_threshold + 1);
							else
								expect_keyword_filled (7);
							end if;

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
						
						when others => command_incomplete (cmd);
					end case;
							
				when others => null;
			end case;
		end draw_route_restrict;

		procedure draw_via_restrict is
			shape : type_shape := to_shape (f (6));
		begin
			case shape is
				when LINE =>
					case fields is
						when 10 =>
							-- board led_driver draw via_restrict [1,3,5-9] line 10 10 60 10
							board_ops.draw_via_restrict_line (
								module_name 	=> module,
								line			=> (
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											start_point	=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											end_point	=> type_point (set (
												x => to_distance (f (9)),
												y => to_distance (f (10))))
											),

								log_threshold	=> log_threshold + 1);

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when ARC =>
					case fields is
						when 13 =>
							-- board led_driver draw via_restrict [1,3,5-9] arc 50 50 0 50 100 0
							board_ops.draw_via_restrict_arc (
								module_name 	=> module,
								arc				=> (
											layers		=> to_layers (f (5)), -- [1,3,5-9]
											center	=> type_point (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											start_point	=> type_point (set (
												x => to_distance (f (9)),
												y => to_distance (f (10)))),
											end_point	=> type_point (set (
												x => to_distance (f (11)),
												y => to_distance (f (12)))),
											direction	=> to_direction (f (13))
											),

								log_threshold	=> log_threshold + 1);

						when 14 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when CIRCLE =>
					case fields is
						when 9 =>
							-- board led_driver draw via_restrict [1,3,5-9] circle 20 50 40
							if is_number (f (7)) then -- 20

								-- Circle is not filled.
								board_ops.draw_via_restrict_circle (
									module_name 	=> module,
									circle			=> 
												(
												layers		=> to_layers (f (5)), -- [1,3,5-9]
												filled		=> NO,
												center	=> type_point (set (
															x => to_distance (f (7)), -- 20
															y => to_distance (f (8)))), -- 50
												radius	=> to_distance (f (9)) -- 40
												),
												
									log_threshold	=> log_threshold + 1);
							else
								expect_value_center_x (7);
							end if;

						when 10 =>
							-- Circle is filled.
							-- board led_driver draw via_restrict [1,3,5-9] circle filled 20 50 40
							if f (7) = keyword_filled then

								-- Circle is filled.
								board_ops.draw_via_restrict_circle (
									module_name 	=> module,
									circle			=> 
												(
												layers		=> to_layers (f (5)), -- [1,3,5-9]
												filled		=> YES,
												center	=> type_point (set (
															x => to_distance (f (8)), -- 20
															y => to_distance (f (9)))), -- 50
												radius	=> to_distance (f (10)) -- 40
												),
												
									log_threshold	=> log_threshold + 1);
							else
								expect_keyword_filled (7);
							end if;

						when 11 .. count_type'last => command_too_long (cmd, fields - 1);
						
						when others => command_incomplete (cmd);
					end case;
							
				when others => null;
			end case;
		end draw_via_restrict;

		procedure draw_stop_mask is
			use et_packages.pac_shapes;
			shape : type_shape := to_shape (f (6));
		begin
			case shape is
				when LINE =>
					case fields is
						when 11 =>
							board_ops.draw_stop_line (
								module_name 	=> module,
								face			=> to_face (f (5)),
								line			=> (
											width		=> to_distance (f (7)),
											start_point	=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
											end_point	=> type_point (set (
												x => to_distance (f (10)),
												y => to_distance (f (11))))
											),

								log_threshold	=> log_threshold + 1);

						when 12 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when ARC =>
					case fields is
						when 14 =>
							board_ops.draw_stop_arc (
								module_name 	=> module,
								face			=> to_face (f (5)),
								arc				=> (
											width	=> to_distance (f (7)),
											center	=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
											start_point	=> type_point (set (
												x => to_distance (f (10)),
												y => to_distance (f (11)))),
											end_point	=> type_point (set (
												x => to_distance (f (12)),
												y => to_distance (f (13)))),
											direction	=> to_direction (f (14))
											),

								log_threshold	=> log_threshold + 1);

						when 15 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when CIRCLE =>
					case fields is
						when 10 =>

						-- The 7th field can either be a line width like 2.5 or a 
						-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
							if is_number (f (7)) then

								-- Circle is not filled and has a circumfence line width
								-- specified in field 7.
								board_ops.draw_stop_circle (
									module_name 	=> module,
									face			=> to_face (f (5)),
									circle			=> 
											(
											filled			=> NO,
											fill_style		=> fill_style_default, -- don't care here
											border_width	=> to_distance (f (7)),
											center			=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
											radius			=> to_distance (f (10))
											),
									log_threshold	=> log_threshold + 1);
							else
								
								-- Circle is filled with the fill style specified in field 7:
								case to_fill_style (f (7)) is
-- CS
-- 										when CUTOUT =>
-- 									
-- 											board_ops.draw_stop_circle (
-- 												module_name 	=> module,
-- 												face			=> to_face (f (5)),
-- 												circle			=> 
-- 															(
-- 															filled		=> YES,
-- 															fill_style	=> CUTOUT,
-- 															center	=> type_point (set (
-- 																		x => to_distance (f (8)),
-- 																		y => to_distance (f (9)))),
-- 															radius	=> to_distance (f (10))
-- 															),
-- 												log_threshold	=> log_threshold + 1);

									when SOLID =>
								
										board_ops.draw_stop_circle (
											module_name 	=> module,
											face			=> to_face (f (5)),
											circle			=> 
														(
														filled		=> YES,
														fill_style	=> SOLID,
														center	=> type_point (set (
																	x => to_distance (f (8)),
																	y => to_distance (f (9)))),
														radius	=> to_distance (f (10))
														),
											log_threshold	=> log_threshold + 1);

									when HATCHED => command_incomplete (cmd);

								end case;
							end if;
								
						when 12 =>
							-- This is going to be a hatched circle.
							-- In this case the 7th field MUST be fill style HATCHED.
							if is_number (f (7)) then
								expect_fill_style (HATCHED, 7); -- error
							else
								case to_fill_style (f (7)) is
									when HATCHED =>
										board_ops.draw_stop_circle (
											module_name 	=> module,
											face			=> to_face (f (5)),
											circle			=> 
													(
													filled		=> YES,
													fill_style	=> HATCHED,
													center		=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
													radius		=> to_distance (f (10)),
													hatching	=> (
																line_width	=> to_distance (f (11)),
																spacing		=> to_distance (f (12)),
																others	=> <>
																)
													),
											log_threshold	=> log_threshold + 1);

									when others => expect_fill_style (HATCHED, 7);
								end case;
							end if;

						when 13 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

							
				when others => null;
			end case;
		end draw_stop_mask;

		procedure draw_stencil is
			use et_packages.pac_shapes;
			shape : type_shape := to_shape (f (6));
		begin
			case shape is
				when LINE =>
					case fields is
						when 11 =>
							board_ops.draw_stencil_line (
								module_name 	=> module,
								face			=> to_face (f (5)),
								line			=> (
											width		=> to_distance (f (7)),
											start_point	=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
											end_point	=> type_point (set (
												x => to_distance (f (10)),
												y => to_distance (f (11))))
											),

								log_threshold	=> log_threshold + 1);

						when 12 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;
					
				when ARC =>
					case fields is
						when 14 =>
							board_ops.draw_stencil_arc (
								module_name 	=> module,
								face			=> to_face (f (5)),
								arc				=> (
											width	=> to_distance (f (7)),
											center	=> type_point (set (
												x => to_distance (f (8)),
												y => to_distance (f (9)))),
											start_point	=> type_point (set (
												x => to_distance (f (10)),
												y => to_distance (f (11)))),
											end_point	=> type_point (set (
												x => to_distance (f (12)),
												y => to_distance (f (13)))),
											direction	=> to_direction (f (14))
											),

								log_threshold	=> log_threshold + 1);

						when 15 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

				when CIRCLE =>
					case fields is
						when 10 =>

						-- The 7th field can either be a line width like 2.5 or a 
						-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
							if is_number (f (7)) then

								-- Circle is not filled and has a circumfence line width
								-- specified in field 7.
								board_ops.draw_stencil_circle (
									module_name 	=> module,
									face			=> to_face (f (5)),
									circle			=> 
											(
											filled			=> NO,
											fill_style		=> fill_style_default, -- don't care here
											border_width	=> to_distance (f (7)),
											center			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
											radius			=> to_distance (f (10))
											),
									log_threshold	=> log_threshold + 1);
							else
								
								-- Circle is filled with the fill style specified in field 7:
								case to_fill_style (f (7)) is
-- CS
-- 										when CUTOUT =>
-- 									
-- 											board_ops.draw_stencil_circle (
-- 												module_name 	=> module,
-- 												face			=> to_face (f (5)),
-- 												circle			=> 
-- 															(
-- 															filled		=> YES,
-- 															fill_style	=> CUTOUT,
-- 															center	=> type_point (set (
-- 																		x => to_distance (f (8)),
-- 																		y => to_distance (f (9)))),
-- 															radius	=> to_distance (f (10))
-- 															),
-- 												log_threshold	=> log_threshold + 1);

									when SOLID =>
								
										board_ops.draw_stencil_circle (
											module_name 	=> module,
											face			=> to_face (f (5)),
											circle			=> 
														(
														filled		=> YES,
														fill_style	=> SOLID,
														center	=> type_point (set (
																	x => to_distance (f (8)),
																	y => to_distance (f (9)))),
														radius	=> to_distance (f (10))
														),
											log_threshold	=> log_threshold + 1);

									when HATCHED => command_incomplete (cmd);

								end case;
							end if;
								
						when 12 =>
							-- This is going to be a hatched circle.
							-- In this case the 7th field MUST be fill style HATCHED.
							if is_number (f (7)) then
								expect_fill_style (HATCHED, 7); -- error
							else
								case to_fill_style (f (7)) is
									when HATCHED =>
										board_ops.draw_stencil_circle (
											module_name 	=> module,
											face			=> to_face (f (5)),
											circle			=> 
													(
													filled		=> YES,
													fill_style	=> HATCHED,
													center		=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
													radius		=> to_distance (f (10)),
													hatching	=> (
																line_width	=> to_distance (f (11)),
																spacing		=> to_distance (f (12)),
																others		=> <>
																)
													),
											log_threshold	=> log_threshold + 1);

									when others => expect_fill_style (HATCHED, 7);
								end case;
							end if;

						when 13 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others => command_incomplete (cmd);
					end case;

							
				when others => null;
			end case;
		end draw_stencil;
		
		-- CS circular tracks are currently not supported
		subtype type_track_shape is type_shape range LINE..ARC;
		
		procedure route_net is 
			shape : type_track_shape := to_shape (f (7));
		begin
			case shape is
				when LINE =>
					if is_number (f (9)) then -- 33.4 or IC4
						
						-- THE TRACK STARTS AT A DEDICATED POINT AT X/Y:
						
						-- board motor_driver route net NET_1 2 line 0.25 0 0 160 0
						case fields is
							when 12 =>
								board_ops.draw_track_line (
									module_name 	=> module,
									net_name		=> to_net_name (f (5)),
									line	=> (
										layer		=> to_signal_layer (f (6)),
										width		=> to_distance (f (8)),
										start_point	=> type_point (set (
											x => to_distance (f (9)),
											y => to_distance (f (10)))),
										end_point	=> type_point (set (
											x => to_distance (f (11)),
											y => to_distance (f (12))))
										),
									
									log_threshold	=> log_threshold + 1
									);

							when 13 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					else
						-- THE TRACK STARTS AT A TERMINAL:
						
						if f (11) = keyword_to then
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
							-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
							
							if is_number (f (12)) then
								-- THE TRACK ENDS AT A DEDICATED POINT X/Y
								
								-- board motor_driver route net NET_1 1 line 0.25 R1 1 to 35 40
								case fields is
									when 13 =>
										board_ops.draw_track_line (
											module_name => module,
											net_name	=> to_net_name (f (5)),
											layer		=> to_signal_layer (f (6)),
											width		=> to_distance (f (8)),
											device		=> to_name (f (9)),
											terminal	=> to_terminal_name (f (10)),
											end_point	=> type_point (set (
													x => to_distance (f (12)),	 -- 35
													y => to_distance (f (13)))), -- 40
											
											log_threshold	=> log_threshold + 1
											);
										
									when 14 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;
										
							else
								-- THE TRACK ENDS ON A GRID LINE ALONG A GIVEN AXIS:
								
								-- board motor_driver route net NET_1 1 line 0.25 R1 1 to x 5
								if f (12) = to_string (X) or f (12) = to_string (Y) then
									case fields is
										when 13 =>
											board_ops.draw_track_line (
												module_name => module,
												net_name	=> to_net_name (f (5)),
												layer		=> to_signal_layer (f (6)),
												width		=> to_distance (f (8)),
												device		=> to_name (f (9)),
												terminal	=> to_terminal_name (f (10)),
												axis		=> to_axis (f (12)),
												notches		=> to_notches (f (13)), -- 5
												
												log_threshold	=> log_threshold + 1
												);
											
										when 14 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;
									
								else
									invalid_keyword (12);
								end if;
							end if;
							
							
						elsif f (11) = keyword_direction then
							-- THE TRACK RUNS INTO GIVEN DIRECTION SPECIFIED BY AN ANGLE
							
							if is_number (f (13)) then
								-- THE TRACK ENDS AFTER A GIVEN DISTANCE (it has a given length)
								
								-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 50
								
								case fields is
									when 13 =>
										board_ops.draw_track_line (
											module_name => module,
											net_name	=> to_net_name (f (5)),
											layer		=> to_signal_layer (f (6)),
											width		=> to_distance (f (8)),
											device		=> to_name (f (9)),
											terminal	=> to_terminal_name (f (10)),
											direction	=> to_rotation (f (12)), -- 45 degree
											length		=> to_distance (f (13)), -- 50mm
											
											log_threshold	=> log_threshold + 1
											);

									when 14 .. count_type'last =>
										command_too_long (cmd, fields - 1);
										
									when others =>
										command_incomplete (cmd);
								end case;

							else
								-- THE TRACK ENDS AT A GIVEN GRID LINE ALONG A GIVEN AXIS
								
								-- board motor_driver route net NET_1 1 line 0.25 R1 1 direction 45 x 5
								if f (13) = to_string (X) or f (13) = to_string (Y) then
									
									case fields is
										when 14 =>
											board_ops.draw_track_line (
												module_name => module,
												net_name	=> to_net_name (f (5)),
												layer		=> to_signal_layer (f (6)),
												width		=> to_distance (f (8)),
												device		=> to_name (f (9)),
												terminal	=> to_terminal_name (f (10)),
												direction	=> to_rotation (f (12)), -- 45 degree
												axis		=> to_axis (f (13)),
												notches		=> to_notches (f (14)), -- 5
												
												log_threshold	=> log_threshold + 1
												);

										when 15 .. count_type'last => command_too_long (cmd, fields - 1);
											
										when others => 
											command_incomplete (cmd);
									end case;
									
								else
									invalid_keyword (13);
								end if;
							end if;

						else
							invalid_keyword (11);
						end if;
					end if;
					
				when ARC =>
					case fields is
						when 15 =>
							-- draw a named track
							board_ops.draw_track_arc (
								module_name 	=> module,
								net_name		=> to_net_name (f (5)),
								arc		=> (
									layer		=> to_signal_layer (f (6)),
									width		=> to_distance (f (8)),
									center		=> type_point (set (
										x => to_distance (f (9)),
										y => to_distance (f (10)))),
									start_point	=> type_point (set (
										x => to_distance (f (11)),
										y => to_distance (f (12)))),
									end_point	=> type_point (set (
										x => to_distance (f (13)),
										y => to_distance (f (14)))),
									direction	=> to_direction (f (15))
									),

								log_threshold	=> log_threshold + 1
								);
							
						when 16 .. count_type'last => command_too_long (cmd, fields - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

			end case;
		end route_net;

		procedure add_layer is
			use et_pcb_stack;
			layer : type_layer;
		begin
			-- board tree_1 add layer 0.12 0.2				
			layer.conductor.thickness := to_distance (f (5));
			layer.dielectric.thickness := to_distance (f (6));
			
			board_ops.add_layer (
				module_name 	=> module,
				layer			=> layer,
				log_threshold	=> log_threshold + 1);

		end add_layer;

		procedure zoom_center is -- GUI related
			-- Build the center point:
			c : type_point := type_point (set (
					x => to_distance (f (5)),
					y => to_distance (f (6))));
		begin
			log (text => "center on point", level => log_threshold + 1);
			center_on (canvas, c);
		end zoom_center;

		procedure set_scale (scale : in string) is  -- GUI related -- CS should be percent of scale_to_fit
			use glib;
			s : gdouble := gdouble'value (scale);
		begin
			log (text => "zoom level", level => log_threshold + 1);
			set_scale (canvas, s);
		end set_scale;
		
		-- Positions the cursor absolute or relative:
		procedure position_cursor is -- GUI related
			use et_geometry;
			
			coordinates : type_coordinates := to_coordinates (f (5));
			position : type_point := type_point (set (
					x => to_distance (f (6)),
					y => to_distance (f (7))));
		begin
			log (text => "place cursor" & to_string (coordinates) 
				& to_string (position), level => log_threshold + 1);
			
			canvas.move_cursor (coordinates, cursor_main, position);
		end position_cursor;
		
	begin -- board_cmd
		log (text => "full command: " & enclose_in_quotes (to_string (cmd)), level => log_threshold);

		domain := to_domain (f (1)); -- DOM_BOARD
		module := to_module_name (f (2)); -- motor_driver (without extension *.mod)

		-- read the verb from field 3
		verb := to_verb (f (3));

		-- There are some very short commands which do not require a verb.
		-- For such commands we do not read the noun.
		case verb is
			when VERB_EXIT | VERB_QUIT => null; -- no noun
			when others => noun := to_noun (f (4)); -- read noun from field 4
		end case;

		-- parse the command:
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_LAYER =>
						case fields is
							when 6 =>
								-- board tree_1 add layer 0.12 0.2
								add_layer;

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);

						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_DELETE =>
				case noun is
					when NOUN_LAYER =>
						case fields is
							when 5 =>
								-- board tree_1 delete layer 2
								board_ops.delete_layer (
									module_name 	=> module,
									layer			=> to_signal_layer (f (5)),
									
									log_threshold	=> log_threshold + 1);

							when 6 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);

						end case;

					when NOUN_OUTLINE =>
						case fields is
							when 7 =>
								-- delete a segment of board outline
								board_ops.delete_outline (
									module_name 	=> module,
									point			=> type_point (set (
											x => to_distance (f (5)),
											y => to_distance (f (6)))),
									accuracy		=> to_distance (f (7)),
									
									log_threshold	=> log_threshold + 1);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_SILKSCREEN =>
						-- board led_driver delete silkscreen top 40 50 1
						case fields is
							when 8 =>
								-- delete a segment of silk screen
								board_ops.delete_silk_screen (
									module_name 	=> module,
									face			=> to_face (f (5)),
									point			=> type_point (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
									accuracy		=> to_distance (f (8)),
									
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;

					when NOUN_ASSY =>
						-- board led_driver delete assy top 40 50 1
						case fields is
							when 8 =>
								-- delete a segment of assembly documentation
								board_ops.delete_assy_doc (
									module_name 	=> module,
									face			=> to_face (f (5)),
									point			=> type_point (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
									accuracy		=> to_distance (f (8)),
									
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_KEEPOUT =>
						-- board led_driver delete keepout top 40 50 1
						case fields is
							when 8 =>
								-- delete a segment of keepout
								board_ops.delete_keepout (
									module_name 	=> module,
									face			=> to_face (f (5)),
									point			=> type_point (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
									accuracy		=> to_distance (f (8)),
									
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_STENCIL =>
						-- board led_driver delete stencil top 40 50 1
						case fields is
							when 8 =>
								-- delete a segment of stencil
								board_ops.delete_stencil (
									module_name 	=> module,
									face			=> to_face (f (5)),
									point			=> type_point (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
									accuracy		=> to_distance (f (8)),
									
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_STOP =>
						-- board led_driver delete stop top 40 50 1
						case fields is
							when 8 =>
								-- delete a segment of stop mask
								board_ops.delete_stop (
									module_name 	=> module,
									face			=> to_face (f (5)),
									point			=> type_point (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
									accuracy		=> to_distance (f (8)),
									
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_ROUTE_RESTRICT =>
						-- board led_driver delete route_restrict 40 50 1
						case fields is
							when 7 =>
								-- delete a segment of route restrict
								board_ops.delete_route_restrict (
									module_name 	=> module,
									point			=> type_point (set (
											x => to_distance (f (5)),
											y => to_distance (f (6)))),
									accuracy		=> to_distance (f (7)),
									
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when NOUN_VIA_RESTRICT =>
						-- board led_driver delete via_restrict 40 50 1
						case fields is
							when 7 =>
								-- delete a segment of via restrict
								board_ops.delete_via_restrict (
									module_name 	=> module,
									point			=> type_point (set (
											x => to_distance (f (5)),
											y => to_distance (f (6)))),
									accuracy		=> to_distance (f (7)),
									
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));

				end case;

			when VERB_DISPLAY => -- GUI related
				case noun is
					when NOUN_GRID => -- like "board led_driver display grid [on/off]"
						case fields is
							when 4 => display_grid; -- if status is omitted
							when 5 => display_grid (f (5));
							when 6 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_SILKSCREEN -- like "board led_driver display silkscreen top [on/off]"
						| NOUN_ASSY | NOUN_KEEPOUT | NOUN_STOP | NOUN_STENCIL =>
						case fields is
							when 5 => display_non_conductor_layer (noun, f (5)); -- if status is omitted
							when 6 => display_non_conductor_layer (noun, f (5), f (6));
							when 7 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when NOUN_CONDUCTORS => -- like "board led_driver display conductors 2 [on/off]"
						case fields is
							when 5 => display_conductor_layer (f (5)); -- if status is omitted
							when 6 => display_conductor_layer (f (5), f (6));
							when 7 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when NOUN_OUTLINE => -- like "board led_driver display outline [on/off]"
						case fields is
							when 4 => display_outline; -- if status is omitted
							when 5 => display_outline (f (5));
							when 6 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_RESTRICT => -- like "board led_driver display restrict route/via 2 [on/off]"
						case fields is
							when 6 => display_restrict_layer (f (5), f (6)); -- if status is omitted
							when 7 => display_restrict_layer (f (5), f (6), f (7));
							when 8 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when NOUN_VIAS => -- like "board led_driver display vias [on/off]"
						case fields is
							--when 5 => display_vias (f (5)); -- if status is omitted
							--when 6 => display_vias (f (5), f (6));
							--when 7 .. count_type'last => too_long;
							when 4 => display_vias; -- if status is omitted
							when 5 => display_vias (f (5));
							when 6 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_DRAW =>
				case noun is
					when NOUN_OUTLINE =>
						declare
							shape : type_shape := to_shape (f (5));
						begin
							case shape is
								when LINE =>
									case fields is
										when 9 =>
											board_ops.draw_outline_line (
												module_name 	=> module,
												line			=> (
													start_point	=> type_point (set (
														x => to_distance (f (6)),
														y => to_distance (f (7)))),
													end_point	=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
													locked		=> lock_status_default
													),
												log_threshold	=> log_threshold + 1
												);

										when 10 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;
									
								when ARC =>
									case fields is
										when 12 =>
											board_ops.draw_outline_arc (
												module_name 	=> module,
												arc				=> (
													center		=> type_point (set (
														x => to_distance (f (6)),
														y => to_distance (f (7)))),
													start_point	=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
													end_point	=> type_point (set (
														x => to_distance (f (10)),
														y => to_distance (f (11)))),
													direction	=> to_direction (f (12)),
													locked	=> lock_status_default
													),

												log_threshold	=> log_threshold + 1
												);

										when 13 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

								when CIRCLE =>
									case fields is
										when 8 =>
											board_ops.draw_outline_circle (
												module_name 	=> module,
												circle			=> (
													center	=> type_point (set (
														x => to_distance (f (6)),
														y => to_distance (f (7)))),
													radius	=> to_distance (f (8)),
													locked	=> lock_status_default
													),

												log_threshold	=> log_threshold + 1
												);

										when 9 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

							end case;
						end;

					when NOUN_SILKSCREEN =>
						declare
							use et_packages.pac_shapes;
							shape : type_shape := to_shape (f (6));
						begin
							case shape is
								when LINE =>
									case fields is
										when 11 =>
											board_ops.draw_silk_screen_line (
												module_name 	=> module,
												face			=> to_face (f (5)),
												line			=> (
															width		=> to_distance (f (7)),
															start_point	=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
															end_point	=> type_point (set (
																x => to_distance (f (10)),
																y => to_distance (f (11))))
															),

												log_threshold	=> log_threshold + 1
												);

										when 12 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;
									
								when ARC =>
									case fields is
										when 14 =>
											board_ops.draw_silk_screen_arc (
												module_name 	=> module,
												face			=> to_face (f (5)),
												arc				=> (
															width	=> to_distance (f (7)),
															center	=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
															start_point	=> type_point (set (
																x => to_distance (f (10)),
																y => to_distance (f (11)))),
															end_point	=> type_point (set (
																x => to_distance (f (12)),
																y => to_distance (f (13)))),
															direction	=> to_direction (f (14))
															),

												log_threshold	=> log_threshold + 1
												);

										when 15 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

								when CIRCLE =>
									case fields is
										when 10 =>

										-- The 7th field can either be a line width like 2.5 or a 
										-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
											if is_number (f (7)) then

												-- Circle is not filled and has a circumfence line width
												-- specified in field 7.
												board_ops.draw_silk_screen_circle (
													module_name 	=> module,
													face			=> to_face (f (5)),
													circle			=> 
															(
															filled			=> NO,
															fill_style		=> fill_style_default, -- don't care here
															border_width	=> to_distance (f (7)),
															center			=> type_point (set (
																		x => to_distance (f (8)),
																		y => to_distance (f (9)))),
															radius			=> to_distance (f (10))
															),
													log_threshold	=> log_threshold + 1);
											else
												
												-- Circle is filled with the fill style specified in field 7:
												case to_fill_style (f (7)) is
-- CS
-- 														when CUTOUT =>
-- 													
-- 															board_ops.draw_silk_screen_circle (
-- 																module_name 	=> module,
-- 																face			=> to_face (f (5)),
-- 																circle			=> 
-- 																			(
-- 																			filled		=> YES,
-- 																			fill_style	=> CUTOUT,
-- 																			center	=> type_point (set (
-- 																						x => to_distance (f (8)),
-- 																						y => to_distance (f (9)))),
-- 																			radius	=> to_distance (f (10))
-- 																			),
-- 																log_threshold	=> log_threshold + 1
-- 																);

													when SOLID =>
												
														board_ops.draw_silk_screen_circle (
															module_name 	=> module,
															face			=> to_face (f (5)),
															circle			=> 
																		(
																		filled		=> YES,
																		fill_style	=> SOLID,
																		center	=> type_point (set (
																					x => to_distance (f (8)),
																					y => to_distance (f (9)))),
																		radius	=> to_distance (f (10))
																		),
															log_threshold	=> log_threshold + 1
															);

													when HATCHED =>
														command_incomplete (cmd);

												end case;
											end if;
												
										when 12 =>
											-- This is going to be a hatched circle.
											-- In this case the 7th field MUST be fill style HATCHED.
											if is_number (f (7)) then
												expect_fill_style (HATCHED, 7); -- error
											else
												case to_fill_style (f (7)) is
													when HATCHED =>
														board_ops.draw_silk_screen_circle (
															module_name 	=> module,
															face			=> to_face (f (5)),
															circle			=> 
																	(
																	filled		=> YES,
																	fill_style	=> HATCHED,
																	center		=> type_point (set (
																				x => to_distance (f (8)),
																				y => to_distance (f (9)))),
																	radius		=> to_distance (f (10)),
																	hatching	=> (
																				line_width	=> to_distance (f (11)),
																				spacing		=> to_distance (f (12)),
																				others		=> <>
																				)
																	),
															log_threshold	=> log_threshold + 1);

													when others =>
														expect_fill_style (HATCHED, 7);
												end case;
											end if;

										when 13 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

											
								when others => null;
							end case;
						end;

					when NOUN_ASSY =>
						declare
							use et_packages.pac_shapes;
							shape : type_shape := to_shape (f (6));
						begin
							case shape is
								when LINE =>
									case fields is
										when 11 =>
											board_ops.draw_assy_doc_line (
												module_name 	=> module,
												face			=> to_face (f (5)),
												line			=> (
															width		=> to_distance (f (7)),
															start_point	=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
															end_point	=> type_point (set (
																x => to_distance (f (10)),
																y => to_distance (f (11))))
															),

												log_threshold	=> log_threshold + 1
												);

										when 12 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;
									
								when ARC =>
									case fields is
										when 14 =>
											board_ops.draw_assy_doc_arc (
												module_name 	=> module,
												face			=> to_face (f (5)),
												arc				=> (
															width	=> to_distance (f (7)),
															center	=> type_point (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
															start_point	=> type_point (set (
																x => to_distance (f (10)),
																y => to_distance (f (11)))),
															end_point	=> type_point (set (
																x => to_distance (f (12)),
																y => to_distance (f (13)))),
															direction	=> to_direction (f (14))
															),

												log_threshold	=> log_threshold + 1
												);

										when 15 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

								when CIRCLE =>
									case fields is
										when 10 =>

										-- The 7th field can either be a line width like 2.5 or a 
										-- fill style like CUTOUT or SOLID. HATCHED is not allowed here:
											if is_number (f (7)) then

												-- Circle is not filled and has a circumfence line width
												-- specified in field 7.
												board_ops.draw_assy_doc_circle (
													module_name 	=> module,
													face			=> to_face (f (5)),
													circle			=> 
															(
															filled			=> NO,
															fill_style		=> fill_style_default, -- don't care here
															border_width	=> to_distance (f (7)),
															center			=> type_point (set (
																		x => to_distance (f (8)),
																		y => to_distance (f (9)))),
															radius			=> to_distance (f (10))
															),
													log_threshold	=> log_threshold + 1);
											else
												
												-- Circle is filled with the fill style specified in field 7:
												case to_fill_style (f (7)) is
-- CS
-- 														when CUTOUT =>
-- 													
-- 															board_ops.draw_assy_doc_circle (
-- 																module_name 	=> module,
-- 																face			=> to_face (f (5)),
-- 																circle			=> 
-- 																			(
-- 																			filled		=> YES,
-- 																			fill_style	=> CUTOUT,
-- 																			center	=> type_point (set (
-- 																						x => to_distance (f (8)),
-- 																						y => to_distance (f (9)))),
-- 																			radius	=> to_distance (f (10))
-- 																			),
-- 																log_threshold	=> log_threshold + 1
-- 																);

													when SOLID =>
												
														board_ops.draw_assy_doc_circle (
															module_name 	=> module,
															face			=> to_face (f (5)),
															circle			=> 
																		(
																		filled		=> YES,
																		fill_style	=> SOLID,
																		center	=> type_point (set (
																					x => to_distance (f (8)),
																					y => to_distance (f (9)))),
																		radius	=> to_distance (f (10))
																		),
															log_threshold	=> log_threshold + 1
															);

													when HATCHED =>
														command_incomplete (cmd);

												end case;
											end if;
												
										when 12 =>
											-- This is going to be a hatched circle.
											-- In this case the 7th field MUST be fill style HATCHED.
											if is_number (f (7)) then
												expect_fill_style (HATCHED, 7); -- error
											else
												case to_fill_style (f (7)) is
													when HATCHED =>
														board_ops.draw_assy_doc_circle (
															module_name 	=> module,
															face			=> to_face (f (5)),
															circle			=> 
																	(
																	filled		=> YES,
																	fill_style	=> HATCHED,
																	center		=> type_point (set (
																				x => to_distance (f (8)),
																				y => to_distance (f (9)))),
																	radius		=> to_distance (f (10)),

																	hatching	=> (
																				line_width	=> to_distance (f (11)),
																				spacing		=> to_distance (f (12)),
																				others		=> <>
																				)
																	),
															log_threshold	=> log_threshold + 1);

													when others =>
														expect_fill_style (HATCHED, 7);
												end case;
											end if;

										when 13 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

											
								when others => null;
							end case;
						end;

					when NOUN_KEEPOUT =>
						draw_keepout;
						
					when NOUN_ROUTE_RESTRICT =>
						draw_route_restrict;

					when NOUN_STENCIL =>
						draw_stencil;
						
					when NOUN_STOP =>
						draw_stop_mask;

					when NOUN_VIA_RESTRICT =>
						draw_via_restrict;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_EXECUTE =>
				case noun is
					when NOUN_SCRIPT =>
						case fields is
							when 5 => 
								execute_nested_script (
									file			=> f (5),
									log_threshold	=> log_threshold + 1);

							when 6 .. count_type'last => too_long;								
							when others => command_incomplete (cmd);
						end case;
							
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_EXIT | VERB_QUIT => terminate_main;
				
			when VERB_FLIP =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 6 =>
								board_ops.flip_device (
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									face			=> to_face  (f (6)),  -- top/bottom
									log_threshold	=> log_threshold + 1
									);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_POSITION => -- GUI related
				case noun is 
					when NOUN_CURSOR =>
						case fields is
							when 7 => position_cursor; -- position cursor absolute/relative 25 30
							when 8 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_ROUTE =>
				case noun is
					when NOUN_FREETRACK =>
						declare
							shape : type_track_shape := to_shape (f (6));
						begin
							case shape is
								when LINE =>
									case fields is
										when 11 =>
											-- draw a freetrack
											board_ops.draw_track_line (
												module_name 	=> module,
												net_name		=> to_net_name (""),
												line	=> (
													width		=> to_distance (f (7)),
													start_point	=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
													end_point	=> type_point (set (
														x => to_distance (f (10)),
														y => to_distance (f (11)))),
													layer		=> to_signal_layer (f (5))
													),
												log_threshold	=> log_threshold + 1
												);

										when 12 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;
									
								when ARC =>
									case fields is
										when 14 =>
											-- draw a freetrack
											board_ops.draw_track_arc (
												module_name 	=> module,
												arc			=> (
													layer			=> to_signal_layer (f (5)),
													width			=> to_distance (f (7)),
													center			=> type_point (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
													start_point		=> type_point (set (
														x => to_distance (f (10)),
														y => to_distance (f (11)))),
													end_point		=> type_point (set (
														x => to_distance (f (12)),
														y => to_distance (f (13)))),
													direction	=> to_direction (f (14))
														),
												net_name		=> to_net_name (""),

												log_threshold	=> log_threshold + 1
												);
											
										when 15 .. count_type'last =>
											command_too_long (cmd, fields - 1);
											
										when others =>
											command_incomplete (cmd);
									end case;

							end case;
						end;

					when NOUN_NET =>
						route_net;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_RIPUP =>
				case noun is
					when NOUN_FREETRACK =>
						case fields is
							when 8 =>
								-- ripup a segment of a freetrack
								board_ops.ripup_track_segment (
									module_name 	=> module,
									net_name		=> to_net_name (""),
									layer			=> to_signal_layer (f (5)),
									point			=> type_point (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
									accuracy		=> to_distance (f (8)),
									
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;

					when NOUN_NET =>
						case fields is
							when 9 =>
								-- ripup a segment of a named track
								board_ops.ripup_track_segment (
									module_name 	=> module,
									net_name		=> to_net_name (f (5)),
									layer			=> to_signal_layer (f (6)),
									point			=> type_point (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
									accuracy		=> to_distance (f (9)),
									
									log_threshold	=> log_threshold + 1
									);

							when 10 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));

				end case;
				
			when VERB_ROTATE =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 7 =>
								board_ops.rotate_device (
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									rotation		=> to_rotation (f (7)),
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_MAKE =>
				case noun is
					when NOUN_PNP =>
						case fields is
							when 4 =>
								board_ops.make_pick_and_place 
									(
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_MOVE =>
				case noun is
					when NOUN_BOARD =>
						case fields is
							when 7 => -- board led_driver move board absolute 20 50
								board_ops.move_board (
									module_name 	=> module,
									coordinates		=> to_coordinates (f (5)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (6)),
														y => to_distance (f (7)))),
									log_threshold	=> log_threshold + 1
									);

							when 8 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;
						
					when NOUN_DEVICE =>
						case fields is
							when 8 =>
								board_ops.move_device (
									module_name 	=> module,
									device_name		=> to_name (f (5)), -- IC1
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;

					when NOUN_SUBMODULE =>
						case fields is
							when 8 =>
								board_ops.move_submodule (
									module_name 	=> module,
									instance		=> et_general.to_instance_name (f (5)), -- OSC1
									coordinates		=> to_coordinates (f (6)),  -- relative/absolute
									point			=> type_point (set (
														x => to_distance (f (7)),
														y => to_distance (f (8)))),
									log_threshold	=> log_threshold + 1
									);

							when 9 .. count_type'last =>
								command_too_long (cmd, fields - 1);
								
							when others =>
								command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

			when VERB_SET =>
				case noun is
					when NOUN_GRID =>
						case fields is
							-- board led_driver set grid 0.5 0.5
							when 6 =>
								board_ops.set_grid (
									module_name 	=> module,
									grid			=> (
											x => to_distance (f (5)),
											y => to_distance (f (6))),
									log_threshold	=> log_threshold + 1);

							when 7 .. count_type'last => command_too_long (cmd, fields - 1);
								
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));

				end case;

-- 			when VERB_SHOW => -- GUI related
-- 				case noun is
-- 					when NOUN_DEVICE =>
-- 						case fields is
-- 							when 5 => null; -- CS
-- 							when 6 .. count_type'last => too_long;
-- 							when others => command_incomplete (cmd);
-- 						end case;
-- 						
-- 					when others => invalid_noun (to_string (noun));
-- 				end case;

			when VERB_ZOOM => -- GUI related
				case noun is
					when NOUN_FIT => -- zoom fit
						case fields is
							when 4 => 
								log (text => "zoom to fit", level => log_threshold + 1);
								scale_to_fit (canvas);

							when 5 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;

					when NOUN_LEVEL => -- zoom level 3
						case fields is
							when 4 => 
								set_scale (f (5));

							when 6 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_CENTER => -- zoom center 10 10
						case fields is
							when 6 =>  -- zoom center 10 10
								zoom_center;

							when 7 =>  -- zoom center 10 10 0.5
								zoom_center;
								set_scale (f (7));

							when 8 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
		end case;
	
		return exit_code;

		exception when event: others => 
		
			log (ERROR, "board command " & enclose_in_quotes (to_string (cmd)) &
				 " invalid !", console => true);

			log (text => ada.exceptions.exception_information (event), console => true);		

		return ERROR;
		
	end board_cmd;
		
	function execute_command (
		file_name		: in pac_script_name.bounded_string; -- for debug messages only
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
		return type_exit_code is

		function f (place : in positive) return string is begin
			return et_string_processing.field (cmd, place);
		end;

		function fields return count_type is begin
			return et_string_processing.field_count (cmd);
		end;
		
		use et_project;
		
		exit_code : type_exit_code := SUCCESSFUL;
		domain	: type_domain; -- DOM_SCHEMATIC
		module	: type_module_name.bounded_string; -- motor_driver (without extension *.mod)

		verb_project	: type_verb_project;
		noun_project	: type_noun_project;

		procedure project_cmd (
			verb : in type_verb_project;
			noun : in type_noun_project) 
		is
			use et_project;
		begin
			case verb is
				when VERB_OPEN =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>
									-- The script command provides the module name only.
									-- The extension must be added here:
									et_project.read_module_file (
										file_name		=> ada.directories.compose (
														name		=> f (4),
														extension	=> module_file_name_extension),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete (cmd);
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;

				when VERB_CREATE =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>

									et_project.create_module (
										module_name		=> to_module_name (f (4)),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete (cmd);
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;

				when VERB_SAVE =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>

									et_project.save_module (
										module_name		=> to_module_name (f (4)),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete (cmd);
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;
					
				when VERB_DELETE =>
					case noun is
						when NOUN_MODULE =>
							case fields is
								when 4 =>

									et_project.delete_module (
										module_name		=> to_module_name (f (4)),
										log_threshold	=> log_threshold + 1
										);

								when 5 .. count_type'last =>
									command_too_long (cmd, fields - 1);
									
								when others => 
									command_incomplete (cmd);
							end case;							
						when others => invalid_noun (to_string (noun));
					end case;
					
			end case;
		end project_cmd;
		
	begin -- execute_command
		log (text => "cmd --> " & enclose_in_quotes (to_string (cmd)), level => log_threshold);
		log_indentation_up;

		-- The command must have at least two fields:
		if field_count (cmd) >= 2 then

			-- field 1 contains the domain of operation
			domain := to_domain (f (1));

			-- Dispatch the command to schematic, board or project:
			case domain is
				when DOM_SCHEMATIC =>
					module := to_module_name (f (2));
					-- CS character and length check

					-- We are inside the project directory right now.
					-- The module should be visible either because it is
					-- in the current project directory or because a environment
					-- variable (like $templates/power_supply.mod) directs to
					-- its real location.
					et_project.read_module_file (
						file_name		=> append_extension (to_string (module)), 
						log_threshold	=> log_threshold + 1); 

					-- The command must have at least four fields.
					if field_count (cmd) >= 4 then
						exit_code := schematic_cmd (cmd, log_threshold + 1);
					else
						command_incomplete (cmd);
					end if;
					
					if exit_code = ERROR then
						raise constraint_error;
					end if;
					
				when DOM_BOARD =>
					module := to_module_name (f (2));
					-- CS character and length check

					-- We are inside the project directory right now.
					-- The module should be visible either because it is
					-- in the current project directory or because a environment
					-- variable (like $templates/power_supply.mod) directs to
					-- its real location.
					et_project.read_module_file (
						file_name		=> append_extension (to_string (module)), 
						log_threshold	=> log_threshold + 1); 

					-- The command must have at least four fields.
					if field_count (cmd) >= 4 then
						exit_code := board_cmd (cmd, log_threshold + 1);
					else
						command_incomplete (cmd);
					end if;

					if exit_code = ERROR then
						raise constraint_error;
					end if;

				when DOM_PROJECT =>

					-- CS test minimum field count
					
					verb_project := to_verb (f (2));
					noun_project := to_noun (f (3));
					
					-- execute rig command
					project_cmd (verb_project, noun_project);
			end case;

		else
			command_incomplete (cmd);
		end if;
		
		log_indentation_down;
		
		return exit_code;

		exception when event: others => 
		
			log (ERROR, "script " & to_string (file_name) & latin_1.space &
				affected_line (cmd) & "command '" &
				to_string (cmd) & "' invalid !", console => true);

			log (text => ada.exceptions.exception_information (event), console => true);		

		return ERROR;

	end execute_command;

	
	function execute_script (
		file_name		: in pac_script_name.bounded_string; -- dummy_module/my_script.scr
		log_threshold	: in type_log_level)
		return type_exit_code is
		
		exit_code : type_exit_code := SUCCESSFUL; -- to be returned

		use ada.directories;

		-- Here we backup the current working directory:
		projects_directory : pac_script_name.bounded_string;

		-- Here we store the directory where the script resides:
		script_directory : pac_script_name.bounded_string;
		
		file_handle : ada.text_io.file_type;
		line : et_string_processing.type_fields_of_line;
		
	begin -- execute_script
		log (text => row_separator_double, level => log_threshold);
		log (text => "executing script " & enclose_in_quotes (to_string (file_name)),
			 level => log_threshold, console => true);
		log_indentation_up;

		-- build the directory where the script is located:
		script_directory := to_script_name (full_name (to_string (file_name)));
		script_directory := to_script_name (containing_directory (to_string (script_directory)));
		
		-- backup the current working directory
		projects_directory := to_script_name (containing_directory (to_string (script_directory)));
		--log (text => "projects directory " & to_string (projects_directory), level => log_threshold);

		-- change in directory where the script is:
		log (text => "changing to directory " & enclose_in_quotes (to_string (script_directory)),
			 level => log_threshold + 1, console => true);
		
		set_directory (to_string (script_directory));

		-- make sure the script file exists:
		if exists (simple_name (to_string (file_name))) then

			-- open script file
			open (
				file => file_handle,
				mode => in_file, 
				name => simple_name (to_string (file_name))); -- demo.scr

			set_input (file_handle);
			
			-- read the file line by line
			while not end_of_file loop
				
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> latin_1.space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then

					-- execute the line as command
					exit_code := execute_command (file_name, line, log_threshold + 1);

					-- evaluate exit code and do things necessary (abort, log messages, ...)
					case exit_code is
						when ERROR => exit;
						when others => null;
					end case;
					
				end if;
			end loop;

		else -- script file not found
			log (ERROR, "script file " & 
				 enclose_in_quotes (simple_name (to_string (file_name))) &
				 " not found !", console => true);
			raise constraint_error;
		end if;
															   
		log (text => "returning to directory " & enclose_in_quotes (to_string (projects_directory)),
			 level => log_threshold + 1);
		set_directory (to_string (projects_directory));
		
		log_indentation_down;
		set_input (standard_input);
		close (file_handle);
		
		return exit_code;

		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			set_input (standard_input);
			--raise;
			return ERROR;
		
	end execute_script;
	
end scripting;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
