------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NET CLASS                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:
--		- separate in two packages things related to board and device package.


with ada.strings.bounded; 			use ada.strings.bounded;

with ada.containers; 				use ada.containers;
with ada.containers.ordered_maps;

with et_board_coordinates;				use et_board_coordinates;
with et_drills;							use et_drills;
with et_design_rules_board;				use et_design_rules_board;


package et_net_class is
	

	net_class_name_length_max : constant positive := 50;
	package pac_net_class_name is new generic_bounded_length (net_class_name_length_max); -- hi-voltage, si-critical, ...
	use pac_net_class_name;
	
	net_class_name_default : constant pac_net_class_name.bounded_string := pac_net_class_name.to_bounded_string ("default");
	
	function to_string (net_class_name : in pac_net_class_name.bounded_string) return string;
	function to_net_class_name (net_class_name : in string) return pac_net_class_name.bounded_string;
	
	net_class_description_length_max : constant positive := 100;
	package pac_net_class_description is new generic_bounded_length (net_class_description_length_max);

	function to_string (class_description : in pac_net_class_description.bounded_string) return string;
	function to_net_class_description (class_description : in string) return pac_net_class_description.bounded_string;
	
	type type_net_class is tagged record
		description				: pac_net_class_description.bounded_string;

		-- The net class parameters assume default values 
		-- that cause minimal manufacturing costs even if 
		-- no net classes have been defined by the operator:
		clearance				: type_track_clearance := 0.3;
		track_width_min			: type_track_width := 0.3;
		via_drill_min			: type_drill_size := 0.3;
		via_restring_min		: type_restring_width := 0.3;
		micro_via_drill_min		: type_drill_size := type_drill_size'last; -- CS use reasonable default
		micro_via_restring_min	: type_restring_width := type_restring_width'last;  -- CS use reasonable default
	end record;

	package pac_net_classes is new ordered_maps (
		key_type		=> pac_net_class_name.bounded_string,
		element_type	=> type_net_class);
	
	
end et_net_class;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
