------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PCB DESIGN RULES                               --
--                                                                          --
--                               S p e c                                    --
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
--   ToDo: 

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with ada.text_io;				use ada.text_io;
with ada.directories;			use ada.directories;

with et_pcb_coordinates;		use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_brd;
with et_string_processing;
with et_terminals;				use et_terminals;
with et_drills;					use et_drills;

package et_design_rules is

	keyword_layout : constant string := "layout";

 	file_name_length_max : constant natural := 100;
	package pac_file_name is new generic_bounded_length (file_name_length_max); -- JLP_ML4_standard.dru
	use pac_file_name;

	function is_empty (rules : in pac_file_name.bounded_string) return boolean;
	
	function to_file_name (file : in string) return pac_file_name.bounded_string;
	function to_string (file : in pac_file_name.bounded_string) return string;

	file_extension : constant string := "dru";


	subtype type_clearance_conductors_of_same_net is type_distance_positive range zero .. type_track_clearance'last;
	
	subtype type_clearance_conductor_to_edge is type_distance_positive range zero .. 0.5;

	subtype type_clearance_edge_to_edge is type_distance_positive range 0.2 .. 1.0;

	type type_clearances is record
		between_conductors			: type_track_clearance := 0.15;
		between_conductors_same_net	: type_clearance_conductors_of_same_net := 0.15;
		conductor_to_board_edge		: type_clearance_conductor_to_edge := 0.3;
		edge_to_edge				: type_clearance_edge_to_edge := 0.3;
	end record;

	type type_restring is record
		outer	: type_restring_width := 0.15;
		inner	: type_restring_width := 0.15;
	end record;
	
	type type_sizes is record
		tracks		: type_track_width := 0.15;
		drills		: type_drill_size := 0.3;
		restring	: type_restring;
	end record;

	subtype type_stop_mask_expansion is type_distance_positive range 0.01 .. 0.2;
	
	type type_stop_mask is record
		expansion_min	: type_stop_mask_expansion;
	end record;
	
	type type_design_rules is record
		clearances	: type_clearances;
		sizes		: type_sizes;
		stop_mask	: type_stop_mask;
	end record;

	package pac_design_rules is new ordered_maps (
		key_type		=> pac_file_name.bounded_string, -- JLP_ML4_standard.dru
		element_type	=> type_design_rules);

	-- here we collect all design rules of the project:
	design_rules : pac_design_rules.map;
	
	procedure read_rules (
		file_name		: in pac_file_name.bounded_string;
		log_threshold 	: in et_string_processing.type_log_level);
	
	keyword_between_conductors			: constant string := "between_conductors";
	keyword_between_conductors_same_net	: constant string := "between_conductors_of_same_net";
	keyword_conductor_to_board_edge		: constant string := "conductor_to_board_edge";
	keyword_edge_to_edge				: constant string := "edge_to_edge";

	keyword_tracks	: constant string := "tracks";
	keyword_drills	: constant string := "drills";
	keyword_inner	: constant string := "inner";
	keyword_outer	: constant string := "outer";

	keyword_expansion_min	: constant string := "expansion_min";
	
	section_clearances		: constant string := "[CLEARANCES";
	section_sizes			: constant string := "[SIZES";
	section_restring		: constant string := "[RESTRING";
	section_stop_mask		: constant string := "[STOP_MASK";
	
	type type_section_name is (
		SEC_INIT,
		SEC_CLEARANCES,
		SEC_SIZES,
		SEC_STOP_MASK,
		SEC_RESTRING
		);

	function to_string (section : in type_section_name) return string;
	-- Converts a section like SEC_CLEARANCES to a string "clearances".

	function get_rules (rules : in pac_file_name.bounded_string)
		return type_design_rules;
		
end et_design_rules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
