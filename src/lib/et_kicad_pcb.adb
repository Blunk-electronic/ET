------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD PCB                            --
--                                                                          --
--                                 ET                                       --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.directories;
with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.exceptions;

with et_general;
with et_libraries;
with et_pcb;
with et_string_processing;		use et_string_processing;

package body et_kicad_pcb is

	function to_package_model (
	-- Builds a package model from the given lines.
		package_name	: in et_libraries.type_component_package_name.bounded_string; -- S_SO14
		lines			: in et_pcb.type_lines.list;
		log_threshold	: in et_string_processing.type_log_level)
		return et_pcb.type_package is
		
		use et_pcb;
		model : type_package; -- to be returned
	
		use et_pcb.type_lines;
		line_cursor : et_pcb.type_lines.cursor := lines.first; -- points to the line being processed

		ob : constant character := '(';
		cb : constant character := ')';

		term_char_seq : constant string (1..2) := latin_1.space & ')';
		term_char_set : character_set := to_set (term_char_seq);

		sec_prefix : constant string (1..4) := "sec_";
		type type_section is (
			SEC_AT,
			SEC_ATTR,
			SEC_ANGLE,
			SEC_CENTER,
			SEC_CLEARANCE,
			SEC_DESCR,
			SEC_DRILL,
			SEC_EFFECTS,
			SEC_END,
			SEC_FONT,
			SEC_FP_ARC,
			SEC_FP_CIRCLE,
			SEC_FP_LINE,
			SEC_FP_TEXT,
			SEC_JUSTIFY,
			SEC_LAYER,
			SEC_LAYERS,
			SEC_MODEL,
			SEC_MODULE,
			SEC_PAD,
			SEC_ROTATE,
			SEC_SCALE,
			SEC_SIZE,
			SEC_SOLDER_MASK_MARGIN,
			SEC_START,
			SEC_TAGS,
			SEC_TEDIT,
			SEC_THICKNESS,
			SEC_WIDTH,
			SEC_XYZ
			);

		function to_string (section : in type_section) return string is
		begin
			return to_lower (type_section'image (section));
		end to_string;

		function expect_keyword (section : in type_section) return string is
			len : positive := to_string (section)'last;
		begin
			return "expect keyword '" & to_string (section)(sec_prefix'last+1 .. len) & "'";
		end expect_keyword;
		
		section : type_section;

		entry_length_max : constant positive := 200;
		package type_argument is new generic_bounded_length (entry_length_max);

-- 		package type_arguments is new doubly_linked_lists (element_type => type_argument.bounded_string);
-- 		use type_arguments;
-- 		arguments : type_arguments.list;


		package type_entry_string is new generic_bounded_length (entry_length_max);
		use type_entry_string;

		type type_entry is record
			entry_string	: type_entry_string.bounded_string;
			line_number		: positive;
		end record;

		package type_entries is new doubly_linked_lists (element_type => type_entry);
		use type_entries;
		entries : type_entries.list;
		entry_cursor : type_entries.cursor;

		procedure next_entry is
		begin
			next (entry_cursor);
		end next_entry;
		
		function the_entry (keep_capitalization : boolean := false) return string is
		begin
			if keep_capitalization then
				return to_string (element (entry_cursor).entry_string);
			else
				return to_lower (to_string (element (entry_cursor).entry_string));
			end if;
		end the_entry;

		function line_number return string is
		begin
			return positive'image (element (entry_cursor).line_number);
		end line_number;

		
-- 		function to_string (arguments : in type_arguments.list) return string is
-- 			arg_cursor : type_arguments.cursor := arguments.first;
-- 			
-- 			package type_result is new 
-- 				generic_bounded_length ((argument_length_max + 1) * positive (length (arguments)));
-- 			use type_result;
-- 				
-- 			argument : type_argument.bounded_string; -- a single argument
-- 			result : type_result.bounded_string; -- lots of arguments to be returned
-- 		begin
-- 			while arg_cursor /= type_arguments.no_element loop
-- 				argument := element (arg_cursor);
-- -- 				log (to_string (argument), log_threshold + 1);
-- 				
-- 				result := result & to_bounded_string (latin_1.space & to_string (argument));
-- 				next (arg_cursor);
-- 			end loop;
-- 
-- 			return to_string (result);
-- 		end to_string;
		
		package sections_stack is new et_general.stack_lifo (max => 20, item => type_section);

		line_length_max : constant positive := 200;
		package type_current_line is new generic_bounded_length (line_length_max);
		use type_current_line;
		current_line : type_current_line.bounded_string;
		character_cursor : natural;

		procedure get_next_line is
		-- Fetches a new line. 
		begin
			next (line_cursor);
			if line_cursor /= et_pcb.type_lines.no_element then
				current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
				log ("line " & to_string (current_line), log_threshold + 3);
			else
				-- no more lines -- CS raise error ?
				null;
			end if;
		end get_next_line;
		
		procedure p1 is
		-- Updates the cursor position to the position of the next
		-- non_space character starting from the current cursor position.
		-- Fetches a new line if no further characters after current cursor position.
		begin
			character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			while character_cursor = 0 loop
				get_next_line;
				character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			end loop;
		end p1;

		procedure read_section is 
		-- Reads the section name from current cursor position until termination
		-- character or its last character.
		-- Stores the section name on sections_stack.
			end_of_kw : integer;  -- may become negative if no terminating character present
		begin
			-- get position of last character
			end_of_kw := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

			-- if no terminating character found, end_of_kw assumes length of line
			if end_of_kw = -1 then
				end_of_kw := length (current_line);
			end if;

			-- Compose section name from cursor..end_of_kw.
			-- This is an implicit general test whether the keyword is a valid keyword.
			section := type_section'value (sec_prefix & slice (current_line, character_cursor, end_of_kw));

			-- update cursor
			character_cursor := end_of_kw;

			-- save section name on stack
			sections_stack.push (section);

			log ("section " & type_section'image (section), log_threshold + 3);

			-- append section name and line number to entries
			append (entries, (entry_string => to_bounded_string (type_section'image (section)),
							  line_number => line_number (element (line_cursor))));
		end read_section;
		

		procedure read_arg is
		-- Reads the argument of a section (or keyword).
		-- Mostly the argument is separated from the section name by space.
		-- Some arguments are wrapped in quotations.
		-- Leaves the cursor at the position of the last character of the argument.
		-- If the argument was enclosed in quotations the cursor is left at
		-- the position of the trailing quotation.
			end_of_arg : integer; -- may become negative if no terminating character present

			use type_argument;
			arg : type_argument.bounded_string; -- here the argument goes temporarily

		begin
			-- We handle an argument that is wrapped in quotation different from a non-wrapped argument:
			if element (current_line, character_cursor) = latin_1.quotation then
				-- Read the quotation-wrapped argument (strip quotations)

				-- get position of last character (before trailing quotation)
				end_of_arg := index (source => current_line, from => character_cursor + 1, pattern => 1 * latin_1.quotation) - 1;

				-- if no trailing quotation found -> error
				if end_of_arg = -1 then
					log_indentation_reset;
					log (message_error & affected_line (element (line_cursor))
						& latin_1.space & latin_1.quotation & " expected");
						raise constraint_error;
				end if;

				-- compose argument from first character after quotation until end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor + 1, end_of_arg));

				-- update cursor (to position of trailing quotation)
				character_cursor := end_of_arg + 1;
			else
				-- Read the argument from current cursor position until termination
				-- character or its last character.

				-- get position of last character
				end_of_arg := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

				-- if no terminating character found, end_of_arg assumes length of line
				if end_of_arg = -1 then
					end_of_arg := length (current_line);
				end if;

				-- compose argument from cursor..end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor, end_of_arg));

				-- update cursor
				character_cursor := end_of_arg;
			end if;

			log ("arg " & to_string (arg), log_threshold + 3);

			-- append argument and line number to entries
			append (entries, (entry_string => to_bounded_string (to_string (arg)),
							  line_number => line_number (element (line_cursor))));

		end read_arg;

		
	begin -- to_package_model
		log ("parsing/building model ...", log_threshold);
		log_indentation_up;

		sections_stack.init;

		-- get first line
		current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
		log ("line " & to_string (current_line), log_threshold + 3);
		
		-- get position of first opening bracket
		character_cursor := type_current_line.index (current_line, 1 * ob);

		loop
			<<label_1>>
				p1;
				read_section;
				p1;
				if element (current_line, character_cursor) = ob then goto label_1; end if;

			<<label_3>>
				read_arg;
				p1;
				-- Test for cb, ob or other character:
				case element (current_line, character_cursor) is

					-- If closing bracket after argument.
					when cb => goto label_2;

					-- If another section at a deeper level follows.
					when ob => goto label_1;

					-- In case another argument follows:
					when others => goto label_3; 
				end case;

			<<label_2>>
				section := sections_stack.pop;

				if sections_stack.depth = 0 then exit; end if;
				p1;

				-- Test for cb, ob or other character:
				case element (current_line, character_cursor) is

					-- If closing bracket after argument.
					when cb => goto label_2;

					-- If another section at a deeper level follows.
					when ob => goto label_1;

					-- In case an argument follows:
					when others => goto label_3; 
				end case;
				
		end loop;

		-- check section name. must be top level section
		if section /= SEC_MODULE then
			log_indentation_reset;
			log (message_error & "top level section not closed !");
			raise constraint_error;
		end if;
		
		-- process entries
		log ("proessing entries ...", log_threshold + 1);
		log_indentation_up;
		entry_cursor := entries.first;

		if the_entry = to_string (SEC_MODULE) then
			next_entry;

			if the_entry (keep_capitalization => true) = et_libraries.to_string (package_name) then
				null;
			else
				log_indentation_reset;
				log (message_error & "expect package name " & et_libraries.to_string (package_name) 
					 & " in line" & line_number
					 & ". Found " & the_entry (keep_capitalization => true) & " !", console => true);
				raise constraint_error;
			end if;
		else
			log_indentation_reset;
			log (message_error & expect_keyword (SEC_MODULE) & " in line" & line_number, console => true);
			raise constraint_error;
		end if;		

		while entry_cursor /= type_entries.no_element loop
			log (the_entry, log_threshold + 2);


			next (entry_cursor);
		end loop;
		log_indentation_down;



		
		log_indentation_down;

		
		return model;
	end to_package_model;
	
	procedure read_libraries (
	-- Reads package libraries. Root directory is et_libraries.lib_dir.
	-- The libraries in the container are named after the libraries found in lib_dir.
		log_threshold 	: in et_string_processing.type_log_level) is

		use ada.directories;
		use et_libraries;
		use et_general;
		use et_general.type_directory_entries;
		use et_pcb;

		-- backup the directory of origin
		use type_directory_name;
		origin_directory : type_directory_name.bounded_string := to_bounded_string (current_directory);
	
		-- After fetching the names of the package libraries, their names
		-- are stored here. When processing the list we use the library_name_cursor.
		library_names : type_directory_entries.list;
		library_name_cursor : type_directory_entries.cursor;

		-- While inserting the libraries the flag library_inserted goes true once
		-- inserting was successuful. It goes false if the library is already in the list.
		-- The library_cursor points to the library in the container package_libraries.
		library_inserted : boolean;
		library_cursor : et_pcb.type_libraries.cursor;

		procedure read_package_names (
		-- Creates empty packages in the package_libraries. The package names are
		-- named after the packages found in the library directories.
			library_name	: in et_pcb.type_library_name.bounded_string;
			packages		: in out type_packages.map) is

			package_names : type_directory_entries.list;
			package_name_cursor : type_directory_entries.cursor;
			
			library_handle : ada.text_io.file_type;
			line : type_fields_of_line; -- a line of a package model

			use et_pcb.type_lines;
			lines : et_pcb.type_lines.list; -- all lines of a single package model

		begin -- read_package_names
			log ("reading package names in " & current_directory & " ...", log_threshold + 3);
			log_indentation_up;

			package_names := directory_entries (
								target_directory	=> current_directory, 
								category			=> ada.directories.ordinary_file,
								pattern				=> package_pattern);

			-- show number of package libraries
			if is_empty (package_names) then
				log (message_warning & "library is empty !");
			else
				log ("found" & count_type'image (length (package_names)) & " packages", log_threshold + 4);
			end if;
			
			log_indentation_up;

			package_name_cursor := package_names.first;
			while package_name_cursor /= type_directory_entries.no_element loop
				log (element (package_name_cursor), log_threshold + 5);
				log_indentation_up;
				
				-- open package model file
				open (
					file => library_handle,
					mode => in_file,
					name => element (package_name_cursor)); -- S_0201.kicad_mod

				-- read lines of model file
				set_input (library_handle);
				while not end_of_file loop
-- 					log (get_line);

					-- Store line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line (
								line 			=> get_line,
								number 			=> ada.text_io.line (current_input),
								--delimiter_wrap	=> true, -- some things are enclosed in quotations
								ifs 			=> latin_1.space); -- fields are separated by space

					-- insert line in container "lines"
					if field_count (line) > 0 then -- we skip empty or commented lines
						append (lines, line);
					end if;
						
				end loop;
				close (library_handle);

				-- From the collected lines the package model can be built and inserted in the 
				-- package list right away:
				type_packages.insert (
					container	=> packages,
					key			=> to_package_name (base_name (element (package_name_cursor))), -- S_0201
					new_item	=> to_package_model (
										package_name 	=> to_package_name (base_name (element (package_name_cursor))), -- S_SO14
										lines			=> lines,
										log_threshold	=> log_threshold + 6));
				
				-- Once the package model file has been read, the collection of lines
				--must be cleared for the next model.
				clear (lines);

				log_indentation_down;
				next (package_name_cursor);
			end loop;

			log_indentation_down;
			log_indentation_down;

			exception
				when event:
					others =>
						log_indentation_reset;
						put_line (ada.exceptions.exception_message (event));
						raise;

		end read_package_names;

	
	begin -- read_libraries
		log ("reading package libraries ...", log_threshold);

		-- fetch package library names from lib_dir
		library_names := directory_entries (
							target_directory	=> et_libraries.to_string (et_libraries.lib_dir), 
							category			=> ada.directories.directory,
							pattern				=> library_pattern);

		log_indentation_up;

		-- Abort if there are no package libraries. Otherwise loop through the library names
		-- and create the libraries in container package_libraries.
		if is_empty (library_names) then
			log_indentation_reset;
			log (message_error & "no package libraries found !");
			raise constraint_error;
		else
			-- show number of package libraries
			log ("found" & count_type'image (length (library_names)) & " libraries", log_threshold + 1);
			log_indentation_up;

			-- Loop through library names and create the actual libraries in container package_libraries:
			library_name_cursor := library_names.first;
			while library_name_cursor /= type_directory_entries.no_element loop
				log ("reading " & element (library_name_cursor) & " ...", log_threshold + 2);

				-- create the (empty) library
				et_pcb.type_libraries.insert (
					container	=> package_libraries,
					key			=> to_library_name (element (library_name_cursor)),
					inserted	=> library_inserted,
					position	=> library_cursor,
					new_item	=> type_packages.empty_map);

				if library_inserted then
					log_indentation_up;
					
					-- change in library (the kicad package library is just a directory like ../lbr/bel_ic.pretty)
					set_directory (compose (to_string (lib_dir), element (library_name_cursor)));
					
					et_pcb.type_libraries.update_element (
						container	=> package_libraries,
						position	=> library_cursor,
						process		=> read_package_names'access);

					-- change back to directory of origin
					set_directory (et_pcb.to_string (origin_directory));
					log_indentation_down;
				else
					log_indentation_up;
					log ("already loaded -> skipped", log_threshold + 2);
					log_indentation_down;
				end if;
				
				next (library_name_cursor);
			end loop;

			log_indentation_down;
		end if;
		
		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					put_line (ada.exceptions.exception_message (event));
					raise;

	end read_libraries;

	

	
end et_kicad_pcb;

-- Soli Deo Gloria
