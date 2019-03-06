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

with ada.tags;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_coordinates;
with et_string_processing;
with et_libraries;
with et_export;
with et_import;
with et_schematic;
with et_pcb;
with et_pcb_coordinates;
with conventions;
with submodules;

package body scripting is

	function to_string (name : in type_script_name.bounded_string) return string is begin
		return type_script_name.to_string (name);
	end;
		
	function to_script_name (name : in string) return type_script_name.bounded_string is begin
		return type_script_name.to_bounded_string (name);
	end;

	function execute_command (
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level)
		return type_exit_code is

		exit_code : type_exit_code := SUCCESSFUL;
	begin
		log ("cmd --> " & to_string (cmd), log_threshold);
		
		return exit_code;
	end execute_command;

	
	function execute_script (
		file_name		: in type_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code is
		exit_code : type_exit_code := SUCCESSFUL;
		file_handle : ada.text_io.file_type;
		line : et_string_processing.type_fields_of_line;
		
	begin -- execute_script
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
				exit_code := execute_command (line, log_threshold + 1);

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
