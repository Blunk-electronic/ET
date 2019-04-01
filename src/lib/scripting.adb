------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET SCRIPTING                              --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            use ada.containers;

with et_general;				use et_general;
with et_string_processing;
with et_project;

with et_coordinates;			use et_coordinates;
with et_libraries;				use et_libraries;
with schematic_ops;
-- with board_ops;


package body scripting is
	
	function to_string (name : in type_script_name.bounded_string) return string is begin
		return type_script_name.to_string (name);
	end;
		
	function to_script_name (name : in string) return type_script_name.bounded_string is begin
		return type_script_name.to_bounded_string (name);
	end;

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
			log (message_error & "domain '" & domain & "' invalid !", console => true);
			raise;
	end;

	function to_string (verb : in type_verb_board) return string is begin
		return type_verb_board'image (verb);
	end;

	function to_verb (verb : in string) return type_verb_board is begin
		return type_verb_board'value (verb);
		exception when event: others => 
			log (message_error & "verb '" & verb & "' invalid !", console => true);
			raise;
	end;
	
	function to_string (verb : in type_verb_schematic) return string is begin
		return type_verb_schematic'image (verb);
	end;

	function to_verb (verb : in string) return type_verb_schematic is begin
		return type_verb_schematic'value (verb);
		exception when event: others => 
			log (message_error & "verb '" & verb & "' invalid !", console => true);
			raise;
	end;
	
	function to_string (noun : in type_noun_schematic) return string is begin
		return type_noun_schematic'image (noun);
	end;

	function to_noun (noun : in string) return type_noun_schematic is begin
		return type_noun_schematic'value (noun);
		exception when event: others => 
			log (message_error & "noun '" & noun & "' invalid !", console => true);
			raise;
	end;

	function to_string (noun : in type_noun_board) return string is begin
		return type_noun_board'image (noun);
	end;

	function to_noun (noun : in string) return type_noun_board is begin
		return type_noun_board'value (noun);
		exception when event: others => 
			log (message_error & "noun '" & noun & "' invalid !", console => true);
			raise;
	end;
	
	function execute_command (
		file_name		: in type_script_name.bounded_string;
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
		return type_exit_code is

		function f (place : in positive) return string is begin
			return et_string_processing.field (cmd, place);
		end;
		
		use et_project;
		
		exit_code : type_exit_code := SUCCESSFUL;
		domain	: type_domain; -- DOM_SCHEMATIC
		module	: type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		
		verb_schematic	: type_verb_schematic;
		noun_schematic	: type_noun_schematic;
		verb_board		: type_verb_board;
		noun_board		: type_noun_board;

		procedure validate_module_name is begin
			if not exists (module) then
				log (message_error & "module " & to_string (module) &
					" not found !", console => true);
				raise constraint_error;
			end if;
		end;
		
		procedure invalid_noun (noun : in string) is begin
			log (message_error & "invalid noun '" & noun & "' for this operation !", console => true);
			raise constraint_error;
		end;
		
		procedure schematic_cmd (verb : in type_verb_schematic; noun : in type_noun_schematic) is
			use et_project;
			use schematic_ops;
		begin
			case verb is
				when ADD =>
					case noun is
						when DEVICE =>
							NULL; -- CS

						when others => invalid_noun (to_string (noun));
					end case;

				when CHECK =>
					case noun is
						when INTEGRITY =>
							schematic_ops.check_integrity (
								module_name 	=> module,
								log_threshold	=> log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;
					
				when DELETE =>
					case noun is
						when DEVICE =>
							schematic_ops.delete_device (
								module_name 	=> module,
								device_name		=> to_device_name (f (5)),
								log_threshold	=> log_threshold + 1);

						when NET =>
							NULL; -- CS

						when TEXT =>
							NULL; -- CS
							
						when UNIT =>
							schematic_ops.delete_unit (
								module_name 	=> module,
								device_name		=> to_device_name (f (5)),
								unit_name		=> to_unit_name (f (6)),
								log_threshold	=> log_threshold + 1);

						when others => invalid_noun (to_string (noun));
					end case;

				when DRAG =>
					case noun is
						when UNIT =>
							schematic_ops.drag_unit
								(
								module_name 	=> module,
								device_name		=> to_device_name (f (5)),
								unit_name		=> to_unit_name (f (6)),
								coordinates		=> schematic_ops.to_coordinates (f (7)), -- relative/absolute
								point			=> et_coordinates.type_point (set_point (
													x => to_distance (f (8)),
													y => to_distance (f (9)))),
								log_threshold	=> log_threshold + 1
								);

						when others => invalid_noun (to_string (noun));
					end case;
					
				when DRAW =>
					case noun is
						when NET =>
							NULL; -- CS

						when others => invalid_noun (to_string (noun));
					end case;

				when INVOKE =>
					case noun is
						when UNIT =>
							NULL; -- CS

						when others => invalid_noun (to_string (noun));
					end case;

				when MOVE =>
					case noun is
						when UNIT_NAME =>
							NULL; -- CS

						when UNIT_VALUE =>
							NULL; -- CS

						when UNIT_PARTCODE =>
							NULL; -- CS

						when UNIT_PURPOSE =>
							NULL; -- CS

						when NET =>
							NULL; -- CS

						when TEXT =>
							NULL; -- CS

						when UNIT =>
							schematic_ops.move_unit
								(
								module_name 	=> module,
								device_name		=> to_device_name (f (5)), -- IC1
								unit_name		=> to_unit_name (f (6)), -- A
								coordinates		=> schematic_ops.to_coordinates (f (7)),  -- relative/absolute
								sheet			=> to_sheet_relative (f (8)),
								point			=> et_coordinates.type_point (set_point (
													x => to_distance (f (9)),
													y => to_distance (f (10)))),
									
								log_threshold	=> log_threshold + 1
								);
							
						when others => invalid_noun (to_string (noun));
					end case;

				when PLACE =>
					case noun is
						when JUNCTION =>
							schematic_ops.place_junction 
								(
								module_name 	=> module,
								place			=> to_coordinates 
													(
													sheet => to_sheet (f (5)),
													point => set_point 
																(
																x => to_distance (f (6)),
																y => to_distance (f (7))
																)
													),
									
								log_threshold	=> log_threshold + 1
								);

						when others => invalid_noun (to_string (noun));
					end case;
					
				when RENAME =>
					case noun is
						when DEVICE =>
							schematic_ops.rename_device
								(
								module_name 		=> module,
								device_name_before	=> to_device_name (f (5)), -- IC1
								device_name_after	=> to_device_name (f (6)), -- IC23
								log_threshold		=> log_threshold + 1
								);

						when NET =>
							NULL; -- CS

						when others => invalid_noun (to_string (noun));
					end case;
					
				when ROTATE =>
					case noun is
						when TEXT =>
							NULL; -- CS

						when UNIT =>
							schematic_ops.rotate_unit
								(
								module_name 	=> module,
								device_name		=> to_device_name (f (5)), -- IC1
								unit_name		=> to_unit_name (f (6)), -- A
								coordinates		=> schematic_ops.to_coordinates (f (7)),  -- relative/absolute
								rotation		=> to_angle (f (8)), -- 90
								log_threshold	=> log_threshold + 1
								);

						when UNIT_NAME =>
							NULL; -- CS

						when UNIT_VALUE =>
							NULL; -- CS

						when UNIT_PARTCODE =>
							NULL; -- CS

						when UNIT_PURPOSE =>
							NULL; -- CS
							
						when others => invalid_noun (to_string (noun));
					end case;

				when SET =>
					case noun is
						when DEVICE_PARTCODE =>
							NULL; -- CS

						when DEVICE_PURPOSE =>
							NULL; -- CS
							
						when DEVICE_VALUE =>
							NULL; -- CS

						when TEXT_SIZE =>
							NULL; -- CS
							
						when others => invalid_noun (to_string (noun));
					end case;

				when WRITE =>
					case noun is
						when TEXT =>
							NULL; -- CS

						when others => invalid_noun (to_string (noun));
					end case;

			end case;
		end schematic_cmd;

		procedure board_cmd (verb : in type_verb_board; noun : in type_noun_board) is
		begin
			null; -- CS
		end board_cmd;
		
	begin -- execute_command
		log ("cmd --> " & to_string (cmd), log_threshold);
		domain := to_domain (f (1));

		case domain is
			when DOM_SCHEMATIC =>
				module := to_module_name (f (2));
				-- CS character and length check
				
				validate_module_name; -- test if module exists

				verb_schematic := to_verb (f (3));
				noun_schematic := to_noun (f (4));

				schematic_cmd (verb_schematic, noun_schematic);
				
			when DOM_BOARD =>
				module := to_module_name (f (2));
				-- CS character and length check
				
				validate_module_name; -- test if module exists

				verb_board := to_verb (f (3));
				noun_board := to_noun (f (4));

				board_cmd (verb_board, noun_board);
		end case;
		
		return exit_code;

		exception when event: others => 
			log (message_error & "script " & to_string (file_name) & latin_1.space &
				affected_line (cmd) & "command '" &
				to_string (cmd) & "' invalid !", console => true);
			return ERROR;

	end execute_command;

	
	function execute_script (
		file_name		: in type_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code is
		exit_code : type_exit_code := SUCCESSFUL;
		file_handle : ada.text_io.file_type;
		line : et_string_processing.type_fields_of_line;
		
	begin -- execute_script
		log (row_separator_double, log_threshold);
		log ("executing script " & to_string (file_name), log_threshold);
		log_indentation_up;

		-- open script file
		open (
			file => file_handle,
			mode => in_file, 
			name => to_string (file_name)); -- demo.scr

		set_input (file_handle);
		
		-- read the file line by line
		while not end_of_file loop
			line := et_string_processing.read_line (
				line 			=> get_line,
				number			=> ada.text_io.line (current_input),
				comment_mark 	=> comment_mark, -- comments start with "--"
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
		
		log_indentation_down;

		set_input (current_input);
		close (file_handle);
		
		return exit_code;

		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			raise;
			return ERROR;


		
	end execute_script;

	
end scripting;
	
-- Soli Deo Gloria
