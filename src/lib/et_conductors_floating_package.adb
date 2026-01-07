------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    FLOATING CONDUCTORS / PACKAGE                         --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
--
-- To Do:
-- - clean up


with ada.text_io;				use ada.text_io;
-- with ada.strings;				use ada.strings;
-- with ada.strings.fixed; 		use ada.strings.fixed;

-- with ada.exceptions;


package body et_conductors_floating_package is
	

	
-- 	procedure validate_track_clearance (clearance : in et_pcb_coordinates.type_distance_model) is
-- 	-- Checks whether the given track clearance is in range of type_track_clearance.
-- 	begin
-- 		if clearance not in type_track_clearance then
-- 			log (ERROR, "track clearance invalid ! Allowed range is" 
-- 				 & to_string (type_track_clearance'first) & " .."
-- 				 & to_string (type_track_clearance'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_track_clearance;
-- 
-- 	procedure validate_track_width (track_width : in type_distance_positive) is
-- 	-- Checks whether the given width is in range of type_track_width.
-- 	begin
-- 		if track_width not in type_track_width then
-- 			log (ERROR, "track width invalid ! Allowed range is" 
-- 				 & to_string (type_track_width'first) & " .."
-- 				 & to_string (type_track_width'last),
-- 				 console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end validate_track_width;



	procedure add_text (
		conductors	: in out type_conductor_objects_both_sides;
		text		: in type_conductor_text;
		face		: in type_face)
	is begin
		case face is
			when TOP =>
				conductors.top.texts.append (text);
				
			when BOTTOM =>
				conductors.bottom.texts.append (text);
		end case;
	end add_text;




	
	procedure mirror_conductor_objects (
		conductors	: in out type_conductor_objects;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		mirror_lines (conductors.lines, axis);
		mirror_arcs (conductors.arcs, axis);
		mirror_circles (conductors.circles, axis);
		mirror_texts (conductors.texts);
	end mirror_conductor_objects;


	procedure rotate_conductor_objects (
		conductors	: in out type_conductor_objects;
		angle		: in type_rotation_model)
	is begin
		rotate_lines (conductors.lines, angle);
		rotate_arcs (conductors.arcs, angle);
		rotate_circles (conductors.circles, angle);
		rotate_texts (conductors.texts, angle);
	end rotate_conductor_objects;

	

	procedure move_conductor_objects (
		conductors	: in out type_conductor_objects;
		offset		: in type_vector_model)
	is begin
		move_lines (conductors.lines, offset);
		move_arcs (conductors.arcs, offset);
		move_circles (conductors.circles, offset);
		move_texts (conductors.texts, offset);
	end move_conductor_objects;

	
	function to_polygons (
		conductors	: in type_conductor_objects;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result, scratch : pac_polygon_list.list;
	begin
		-- lines:
		result := to_polygons (conductors.lines, tolerance);

		-- arcs:
		scratch := to_polygons (conductors.arcs, tolerance);
		result.splice (before => pac_polygon_list.no_element, source => scratch);

		-- circles (outer edges only ):
		scratch := to_polygons_outside (conductors.circles, tolerance);
		result.splice (before => pac_polygon_list.no_element, source => scratch);
		
		-- texts
		scratch := to_polygons (conductors.texts);
		result.splice (before => pac_polygon_list.no_element, source => scratch);
		
		return result;
	end to_polygons;


	
end et_conductors_floating_package;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
