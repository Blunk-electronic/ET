------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / FILE READ / WRITE                      --
--                                                                          --
--                               S p e c                                    --
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


with et_string_processing;			use et_string_processing;

with et_geometry_2a;
with et_geometry_2a.contours;


generic
	with package pac_geometry is new et_geometry_2a (<>);
	with package pac_contours is new pac_geometry.contours;
	
package et_geometry_2_file_rw is

	use pac_geometry;
	use pac_contours;
	
	

	-- writes start and end point of a line
	procedure write_line (line : in type_line'class);

	-- writes center, start and end point of an arc
	procedure write_arc (arc : in type_arc'class);

	
	-- writes center and radius of a circle
	procedure write_circle (circle : in type_circle'class);


	-- writes the segments of a polygon (lines, arcs or a single circle):
	procedure write_polygon_segments (
		polygon : in type_contour'class);


	
end et_geometry_2_file_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
