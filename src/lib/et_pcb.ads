------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET PCB                                  --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

with et_string_processing;
with et_libraries;				use et_libraries;
with et_pcb_coordinates;		use et_pcb_coordinates;

package et_pcb is

	-- If lines of a file are to be collected we use this simple list:
	package type_lines is new doubly_linked_lists (
		element_type => et_string_processing.type_fields_of_line,
		"=" => et_string_processing.lines_equally);
	
	use et_libraries.type_component_package_name;

	directory_name_length_max : constant positive := 200;
	package type_directory_name is new generic_bounded_length (directory_name_length_max);

	function to_string (directory_name : in type_directory_name.bounded_string) return string;
	-- Converts a directory name to a string.

-- 	function to_directory (directory_name : in string) return type_directory_name.bounded_string;
-- 	-- Converts a string to a type_directory_name.

	
	
	type type_assembly_technology is (
		THT,	-- Through Hole Technology
		SMT		-- Surface Mount Technology
		);

	function to_string (technology : in type_assembly_technology) return string;

	type type_terminal_shape is (CIRCULAR, NON_CIRCULAR);
	
	type type_terminal_shape_tht is (OCTAGON, CIRCULAR, RECTANGLE, LONG, LONG_OFFSET);
	function to_string (shape : in type_terminal_shape_tht) return string;
	
	type type_terminal_shape_smt is (RECTANGLE, CIRCULAR, LONG);
	function to_string (shape : in type_terminal_shape_smt) return string;	

	type type_terminal_solder_paste is (NONE, APPLIED);
	function to_string (solder_paste : in type_terminal_solder_paste) return string;
	
	type type_terminal_stop_mask is (CLOSED, OPEN);
	function to_string (stop_mask : in type_terminal_stop_mask) return string;

	type type_terminal_tht_hole is (DRILLED, MILLED);
	
	type type_terminal (
		technology	: type_assembly_technology;
		shape		: type_terminal_shape;
		tht_hole	: type_terminal_tht_hole) -- without meaning if technology is SMT
	is record
		position	: type_terminal_position;

		case technology is
			when THT =>
				-- restring_outer_layer : type_distance; -- CS use subtype for reasonable range
				width_inner_layers : type_distance; -- CS use subtype for reasonable range
				
				shape_tht 	: type_terminal_shape_tht;

				case shape is
					when CIRCULAR =>
						drill_size_cir : type_distance; -- CS use subtype for reasonable range
						
					WHEN NON_CIRCULAR =>
						size_tht_x, size_tht_y : type_distance;  -- CS use subtype for reasonable range

						case tht_hole is
							when DRILLED =>
								drill_size_dri : type_distance; -- CS use subtype for reasonable range

							when MILLED =>
								null; -- cs
						end case;
						
				end case;

				
			when SMT =>
				shape_smt		: type_terminal_shape_smt;
				face			: type_face;
				stop_mask 		: type_terminal_stop_mask;
				solder_paste	: type_terminal_solder_paste;
				case shape is
					when CIRCULAR =>
						null;
						
					WHEN NON_CIRCULAR =>
						size_smt_x, size_smt_y : type_distance;  -- CS use subtype for reasonable range
				end case;

		end case;
	end record;

	package type_terminals is new indefinite_ordered_maps (
		key_type		=> type_terminal_name.bounded_string,
		element_type	=> type_terminal,
		"<"				=> type_terminal_name."<");


	type type_package is record
		terminals	: type_terminals.map;
	end record;

	
	--type type_component_package (technology : type_package_technology) is record
	--type type_component_package (category : type_component_category) is record
-- 	type type_component_package is record
-- 		name 			: type_component_package_name.bounded_string; -- S_SOT23
-- 		library			: type_full_library_name.bounded_string; -- projects/lbr/smd_packages.pac
-- 		terminal_count	: type_terminal_count; -- 14
-- 		case technology is
-- 			when THT => 
-- 				pad_tht_count : positive; -- 14 for a NDIP14 package
-- 
-- 			when SMT => 
-- 				pad_smt_count : positive; -- 3 for a S_SOT23 package
-- 
-- 			when THT_SMT => 
-- 				pad_tht_count : positive; -- 8 for heat dissipation
-- 				pad_smt_count : positive; -- 32 for a TSSOP32 package
-- 		end case;
-- 	end record;

	package type_packages is new indefinite_ordered_maps ( -- CS try ordered_maps instead
		key_type 		=> type_component_package_name.bounded_string, -- S_SO14
		element_type 	=> type_package);
	use type_packages;

	package type_libraries is new ordered_maps (
		key_type		=> type_full_library_name.bounded_string, -- projects/lbr/smd_packages.pac
		element_type	=> type_packages.map,
		"<"				=> type_full_library_name."<");

	-- All package models are collected here:
	package_libraries : type_libraries.map;


	procedure terminal_properties (
	-- Logs the properties of the terminal indicated by cursor.
		cursor 			: in type_terminals.cursor;
		log_threshold 	: in et_string_processing.type_log_level);

	

	function terminal_count (
		library_name		: in type_full_library_name.bounded_string;
		package_name 		: in type_component_package_name.bounded_string)
		return et_libraries.type_terminal_count;

	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in type_full_library_name.bounded_string;
		package_name 		: in type_component_package_name.bounded_string;
		terminal_port_map	: in type_terminal_port_map.map) 
		return boolean;

		
end et_pcb;

-- Soli Deo Gloria
