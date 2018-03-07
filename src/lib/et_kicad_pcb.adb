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

	procedure read_libraries_names (
	-- Reads package libraries. Root directory is et_libraries.lib_dir.
	-- Creates empty libraries in container package_libraries.
	-- The libraries in the container are named after the libraries found in lib_dir.
		log_threshold 	: in et_string_processing.type_log_level) is

		use et_general;
		use et_general.type_directory_entries;
		use et_pcb;

		-- After fetching the names of the package libraries, their names
		-- are stored here. When processing the list we use the library_name_cursor.
		package_library_names : type_directory_entries.list;
		library_name_cursor : type_directory_entries.cursor;

		-- While inserting the libraries the flag library_inserted goes true once
		-- inserting was successuful. It goes false if the library is already in the list.
		-- The library_cursor points to the library in the container package_libraries.
		library_inserted : boolean;
		library_cursor : type_libraries.cursor;
	
	begin -- read_libraries_names
		log ("Loading package libraries ...", log_threshold);

		-- fetch package library names from lib_dir
		package_library_names := directory_entries (
								target_directory	=> et_libraries.to_string (et_libraries.lib_dir), 
								category			=> ada.directories.directory,
								pattern				=> package_library_pattern);

		log_indentation_up;

		-- Abort if there are no package libraries. Otherwise loop through the library names
		-- and create the libraries in container package_libraries.
		if is_empty (package_library_names) then
			log_indentation_reset;
			log (message_error & "no package libraries found !");
			raise constraint_error;
		else
			-- show number of package libraries
			log ("found" & count_type'image (length (package_library_names)) & " libraries", log_threshold + 1);
			log_indentation_up;

			-- Loop through library names and create the actual libraries in container package_libraries:
			library_name_cursor := package_library_names.first;
			while library_name_cursor /= type_directory_entries.no_element loop
				log ("reading " & element (library_name_cursor) & " ...", log_threshold + 2);

				-- create the (empty) library
				type_libraries.insert (
					container	=> package_libraries,
					key			=> to_library_name (element (library_name_cursor)),
					inserted	=> library_inserted,
					position	=> library_cursor,
					new_item	=> type_packages.empty_map);

				if not library_inserted then
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

	end read_libraries_names;

	
	procedure read_package_names (
	-- Creates empty packages in the package_libraries. The package names are
	-- named after the packages found in the library directories.
		log_threshold 	: in et_string_processing.type_log_level) is

		use et_general;
		use et_general.type_directory_entries;
		use et_pcb;

		use et_pcb.type_libraries;
		-- This cursor points to the library being processed:
		library_cursor : type_libraries.cursor := package_libraries.first;

	begin -- read_package_names
		log ("Loading package names ...", log_threshold);
		log_indentation_up;

		while library_cursor /= type_libraries.no_element loop
			log ("library " & to_string (library_name => key (library_cursor)), log_threshold + 1);

			next (library_cursor);
		end loop;

		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					put_line (ada.exceptions.exception_message (event));
					raise;

	end read_package_names;
	
		

	procedure read_package_libraries (
		log_threshold 	: in et_string_processing.type_log_level) is
	begin
		read_libraries_names (log_threshold);
		read_package_names (log_threshold);
	end read_package_libraries;
	
end et_kicad_pcb;

-- Soli Deo Gloria
