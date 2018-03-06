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
with et_libraries;

package et_pcb is

	type type_axis is (X, Y, Z);
	type type_face is (TOP, BOTTOM);
	
	-- The total distance between two objects:
	type type_distance_total is delta 0.001 range -100_000_000.00 .. 100_000_000.00; -- unit is metric millimeter
	for type_distance_total'small use 0.001; -- this is the accuracy required for layout

	-- The x and y position of an object:
	subtype type_distance is type_distance_total range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter
	zero_distance : constant type_distance := 0.0;
	
	type type_angle is delta 0.01 range -359.9 .. 359.9;
	for type_angle'small use 0.01;
	zero_angle : constant type_angle := 0.0;

	type type_position is tagged private;
	type type_position_placement is private;

	function position_placement_default return type_position_placement;

	type type_package is record
		dummy : natural;
	end record;

	use et_libraries.type_component_package_name;
	
	package type_packages is new indefinite_ordered_maps (
		key_type 		=> et_libraries.type_component_package_name.bounded_string, -- S_SO14
		element_type 	=> type_package);



	
	-- All package models are collected here:
	package_libraries : type_packages.map;




	
	type type_package_technology is (
		THT,		-- Through Hole Technology
		SMT,		-- Surface Mount Technology
		THT_SMT		-- mixed THT & SMT
		);

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

	function terminal_count (
		library_name		: in et_libraries.type_full_library_name.bounded_string;
		package_name 		: in et_libraries.type_component_package_name.bounded_string)
		return et_libraries.type_terminal_count;

	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in et_libraries.type_full_library_name.bounded_string;
		package_name 		: in et_libraries.type_component_package_name.bounded_string;
		terminal_port_map	: in et_libraries.type_terminal_port_map.map) 
		return boolean;

	private

		type type_position is tagged record
			x, y, z : type_distance := zero_distance;
		end record;

		zero : constant type_position := (others => zero_distance);

		type type_position_placement is new type_position with record
			face	: type_face := TOP;
			angle	: type_angle := zero_angle;
		end record;

		
end et_pcb;

-- Soli Deo Gloria
