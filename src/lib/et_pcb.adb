------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET PCB                                  --
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

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.exceptions;

with et_libraries;
with et_string_processing;		use et_string_processing;

package body et_pcb is


	function to_string (directory_name : in type_directory_name.bounded_string) return string is
	-- Converts a directory name to a string.
	begin
		return type_directory_name.to_string (directory_name);
	end to_string;
	
	function to_directory (directory_name : in string) return type_directory_name.bounded_string is
	-- Converts a string to a type_directory_name.
	begin
		return type_directory_name.to_bounded_string (directory_name);
	end to_directory;

	function to_string (technology : in type_assembly_technology) return string is
	begin
		return type_assembly_technology'image (technology);
	end to_string;

	function to_string (shape : in type_terminal_shape_tht) return string is
	begin
		return type_terminal_shape_tht'image (shape);
	end to_string;

	function to_string (shape : in type_terminal_shape_smt) return string is
	begin
		return type_terminal_shape_smt'image (shape);
	end to_string;

	function to_string (solder_paste : in type_terminal_solder_paste) return string is
	begin
		return type_terminal_solder_paste'image (solder_paste);
	end to_string;

	function to_string (stop_mask : in type_terminal_stop_mask) return string is
	begin
		return type_terminal_stop_mask'image (stop_mask);
	end to_string;

	function no_contour return type_package_contour is
	-- Returns an empty package contour.		
	begin
		return (
			lines 	=> type_package_contour_lines.empty_list,
			arcs 	=> type_package_contour_arcs.empty_list,
			circles	=> type_package_contour_circles.empty_list);
	end no_contour;
	
	procedure terminal_properties (
	-- Logs the properties of the terminal indicated by cursor.
		cursor 			: in type_terminals.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is
		use type_terminals;
		use et_pcb_coordinates;
	begin
		log ("terminal name " & to_string (key (cursor))
			& " technology " & to_string (element (cursor).technology)
			& to_string (type_point_3d (element (cursor).position))
			& to_string (angle => get_angle (element (cursor).position), preamble => true),
			log_threshold + 1);

		log_indentation_up;
		case element (cursor).technology is
			when THT => 
				log ("shape " & to_string (element (cursor).shape_tht), log_threshold + 2);
				log ("copper with inner layers " & to_string (element (cursor).width_inner_layers), log_threshold + 2);
				case element (cursor).shape is
					when NON_CIRCULAR =>
						log ("size x " & to_string (element (cursor).size_tht_x), log_threshold + 2);
						log ("size y " & to_string (element (cursor).size_tht_y), log_threshold + 2);
						case element (cursor).tht_hole is
							when DRILLED =>
								log ("drill " & to_string (element (cursor).drill_size_dri), log_threshold + 2); 
							when MILLED =>
								log ("plated milling contour ", log_threshold + 2);
						end case;
						
					when CIRCULAR =>
						log ("drill " & to_string (element (cursor).drill_size_cir), log_threshold + 2); 
				end case;
				
			when SMT => 
				log ("shape " & to_string (element (cursor).shape_smt), log_threshold + 2);

				case element (cursor).shape is
					when NON_CIRCULAR =>
						log ("size x " & to_string (element (cursor).size_smt_x), log_threshold + 2);
						log ("size y " & to_string (element (cursor).size_smt_y), log_threshold + 2);
					when CIRCULAR =>
						null; -- CS 
				end case;
				
				log ("face " & to_string (element (cursor).face), log_threshold + 2);
				log ("stop mask " & to_string (element (cursor).stop_mask), log_threshold + 2);
				log ("solder paste " & to_string (element (cursor).solder_paste), log_threshold + 2);
		end case;

		log_indentation_down;
	end terminal_properties;

	
	function terminal_count (
	-- Returns the number of terminals of the given package in the given library.
		library_name		: in type_full_library_name.bounded_string;
		package_name 		: in type_component_package_name.bounded_string)
		return et_libraries.type_terminal_count is

		use type_libraries;
		
		terminals : et_libraries.type_terminal_count; -- to be returned
		library_cursor : type_libraries.cursor; -- points to the library

		procedure locate_package (
			library_name	: in type_full_library_name.bounded_string;
			packages		: in type_packages.map) is
			use type_terminals;
			package_cursor : type_packages.cursor;
		begin
			-- locate the package
			package_cursor := packages.find (package_name);

			-- get number of terminals
			terminals := type_terminal_count (length (element (package_cursor).terminals));
		end locate_package;
		
	begin -- terminal_count
		-- locate the library
		library_cursor := type_libraries.find (package_libraries, library_name);

		if library_cursor = type_libraries.no_element then
			log_indentation_reset;
			log (message_error & to_string (library_name) & " not found !", console => true);
			raise constraint_error;
		else
			-- query packages in library
			type_libraries.query_element (library_cursor, locate_package'access);
		end if;
		
		return terminals;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (ada.exceptions.exception_message (event), console => true);
					raise;

	end terminal_count;
	
	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in type_full_library_name.bounded_string;
		package_name 		: in type_component_package_name.bounded_string;
		terminal_port_map	: in type_terminal_port_map.map) 
		return boolean is
	begin
		return true; -- CS
	end terminal_port_map_fits;

	
end et_pcb;

-- Soli Deo Gloria
