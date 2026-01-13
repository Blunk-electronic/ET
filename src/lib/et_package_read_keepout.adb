------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PACKAGE READ / KEEPOUT                           --
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

with ada.text_io;						use ada.text_io;
with ada.strings; 						use ada.strings;

with et_design_rules_board;				use et_design_rules_board;
with et_board_geometry;					use et_board_geometry;

with et_primitive_objects;				use et_primitive_objects;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_keywords;						use et_keywords;
with et_package_model;					use et_package_model;
with et_directions;						use et_directions;

with et_keepout;						use et_keepout;
with et_keepout.packages;				use et_keepout.packages;

with et_package_read_contour;			use et_package_read_contour;


package body et_package_read_keepout is

	use pac_geometry_2;
	use pac_contours;


	
	procedure insert_keepout_zone (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is begin
		add_zone (packge.keepout, (contour with null record), face);
		
		-- clean up for next contour
		reset_contour (contour);
	end;




	procedure insert_keepout_zone_cutout (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is begin
		add_cutout (packge.keepout, (contour with null record), face);
		
		-- clean up for next contour
		reset_contour (contour);
	end;

	
	
end et_package_read_keepout;
