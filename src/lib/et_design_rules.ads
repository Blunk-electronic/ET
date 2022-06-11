------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PCB DESIGN RULES                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
use et_pcb_coordinates.pac_geometry_2;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
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


	conductor_width_min : constant type_distance_positive := 0.05;
	
	conductor_clearance_min : constant type_distance_positive := conductor_width_min;

	-- The clearance between two conductor objects:
	subtype type_track_clearance is type_distance_positive  -- CS rename to type_clearance_conductor
		range conductor_clearance_min .. type_distance_positive'last;

	-- Checks whether the given track clearance is in range of type_track_clearance.
	procedure validate_track_clearance (clearance : in type_distance);

	

	subtype type_clearance_conductors_of_same_net is type_distance_positive range zero .. type_track_clearance'last;
	
	subtype type_clearance_conductor_to_edge is type_distance_positive range zero .. 0.5;

	subtype type_clearance_edge_to_edge is type_distance_positive range 0.2 .. 1.0;

	type type_clearances is record
		between_conductors			: type_track_clearance := 0.15;
		between_conductors_same_net	: type_clearance_conductors_of_same_net := 0.15;
		conductor_to_board_edge		: type_clearance_conductor_to_edge := 0.3;
		edge_to_edge				: type_clearance_edge_to_edge := 0.3;
	end record;


	dru_parameter_clearance_conductor_to_board_edge : constant string := 
		"clearance conductor to board edge [mm]";

	-- CS: other clearances. see above


	
	
-- RESTRING
	
	restring_width_max : constant type_distance_positive := 5.0;
	subtype type_restring_width is type_distance_positive 
		range conductor_width_min .. restring_width_max;

	
	-- Some PCB manufacturers make the inner restring slightly
	-- wider than the outer. So we require a type for the
	-- delta between inner an outer restring:
	subtype type_restring_delta_inner_outer is type_distance_positive
		range 0.0 .. type_restring_width'last;
	
	type type_restring is record
		outer		: type_restring_width := 0.15;
		delta_size	: type_restring_delta_inner_outer := 0.0;
		inner		: type_restring_width := 0.15;
	end record;

	type type_restring_category is (INNER, OUTER);


	drill_to_restring_multiplier : constant type_distance_positive := 0.5;
	
	-- Calculates the width of the restring as follows:
	-- If category is OUTER then the formula is:
	--  restring : drill_size * drill_to_restring_multiplier
	-- If category is INNER then the formula is:
	--  restring : drill_size * drill_to_restring_multiplier + delta_size
	-- So delta_size is not relevant if category is OUTER.
	function auto_set_restring (
		category	: in type_restring_category; -- inner/outer
		drill_size	: in type_drill_size;
		delta_size	: in type_restring_delta_inner_outer := zero)
		return type_restring_width;

	-- Checks whether the given restring width is in range of type_restring_width.
	procedure validate_restring_width (
		restring_width : in type_distance);


	
	
	track_width_max : constant type_distance_positive := 100.0;
	
	subtype type_track_width is type_distance_positive 
		range conductor_width_min .. track_width_max;

	-- Checks whether the given track width is in range of type_track_width.
	procedure validate_track_width (
		track_width : in type_distance);

	
	
	type type_sizes is record
		tracks		: type_track_width := 0.15;
		drills		: type_drill_size := 0.3;
		restring	: type_restring;
	end record;

	

	stop_mask_expansion_min : constant type_distance_positive := 0.01;
	stop_mask_expansion_max : constant type_distance_positive := 0.2;
	subtype type_stop_mask_expansion is type_distance_positive
		range stop_mask_expansion_min .. stop_mask_expansion_max;
	-- see <https://docs.oshpark.com/tips+tricks/stop-mask-expansion/>

	
	type type_stop_mask is record
		expansion_min	: type_stop_mask_expansion := 0.075;
	end record;


	
	type type_design_rules is record
		clearances	: type_clearances;
		sizes		: type_sizes;
		stop_mask	: type_stop_mask;
	end record;

	design_rules_default : constant type_design_rules := (others => <>);
	
	package pac_design_rules is new ordered_maps (
		key_type		=> pac_file_name.bounded_string, -- JLP_ML4_standard.dru
		element_type	=> type_design_rules);

	-- Here we collect all sets of design rules of the project:
	design_rules : pac_design_rules.map;

	
	-- Reads the design rulesspecified in the given file
	-- and inserts the data set in list "design_rules" (see above):
	procedure read_rules (
		file_name		: in pac_file_name.bounded_string;
		log_threshold 	: in type_log_level);

	
	keyword_between_conductors			: constant string := "between_conductors";
	keyword_between_conductors_same_net	: constant string := "between_conductors_of_same_net";
	keyword_conductor_to_board_edge		: constant string := "conductor_to_board_edge";
	keyword_edge_to_edge				: constant string := "edge_to_edge";

	keyword_tracks	: constant string := "tracks";
	keyword_drills	: constant string := "drills";
	keyword_inner	: constant string := "inner";
	keyword_outer	: constant string := "outer";
	keyword_delta	: constant string := "delta";

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


	-- Returns the design rule data set specified in given rules file.
	-- If the given rules file does not exist (of if rules is empty)
	-- returns default rules.
	function get_rules (rules : in pac_file_name.bounded_string) -- JLP_ML4_standard.dru
		return type_design_rules;
		
end et_design_rules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
