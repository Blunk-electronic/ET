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
with ada.strings.maps;			use ada.strings.maps;
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
			package_cursor : type_directory_entries.cursor;
			
			library_handle : ada.text_io.file_type;
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

			package_cursor := package_names.first;
			while package_cursor /= type_directory_entries.no_element loop
				log (element (package_cursor), log_threshold + 5);

				open (
					file => library_handle,
					mode => in_file,
					name => element (package_cursor));

				set_input (library_handle);
				
				while not end_of_file loop
					log (get_line);

-- 					-- Store line in variable "line" (see et_string_processing.ads)
-- 					line := et_string_processing.read_line (
-- 								line 			=> get_line,
-- 								number 			=> ada.text_io.line (current_input),
-- 								comment_mark 	=> "", -- there are no comment marks in package model file
-- 								ifs 			=> latin_1.space); -- fields are separated by space
					
				end loop;
				
				close (library_handle);
				
				next (package_cursor);
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
