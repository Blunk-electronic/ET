------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              DRILLS                                      --
--                                                                          --
--                              S p e c                                     --
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

with et_pcb_coordinates_2;		use et_pcb_coordinates_2;
with et_object_status;			use et_object_status;
with et_logging;				use et_logging;


package et_drills is
	use pac_geometry_2;
	
	
	-- We fit the diameter in a reasonable range via a subtype:
	drill_size_min : constant type_distance_positive := 0.05;
	drill_size_max : constant type_distance_positive := 10.0;
	subtype type_drill_size is type_distance_positive range drill_size_min .. drill_size_max;

	
	-- Checks whether given drill size is in range of type_drill_size
	procedure validate_drill_size (drill : in type_distance_model);

	-- DRILLS
	type type_drill is tagged record
		position	: type_vector_model;
		diameter	: type_drill_size := drill_size_min;
		status		: type_object_status;
	end record;

	
	-- returns the properties of the given drill as string:
	function to_string (drill : in type_drill) return string;


	function get_position (
		drill : in type_drill)
		return type_vector_model;
	

	-- Returns true if the drill is inside the given zone:
	function in_catch_zone (
		zone	: in type_catch_zone;
		drill	: in type_drill)
		return boolean;
	

	function is_selected (
		drill : in type_drill)
		return boolean;

	procedure set_selected (
		drill : in out type_drill);

	procedure clear_selected (
		drill : in out type_drill);

	
	function is_proposed (
		drill : in type_drill)
		return boolean;

	procedure set_proposed (
		drill : in out type_drill);

	procedure clear_proposed (
		drill : in out type_drill);

	
	function is_moving (
		drill : in type_drill)
		return boolean;

	procedure set_moving (
		drill : in out type_drill);

	procedure clear_moving (
		drill : in out type_drill);

	
	procedure modify_status (
		drill 		: in out type_drill;
		operation	: in type_status_operation);

	

	procedure reset_status (
		drill : in out type_drill);



	

	subtype type_drill_size_tht is type_drill_size range 0.8 .. 5.0;

	
end et_drills;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
