------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              TESTBENCH                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- DESCRIPTION:
-- 

with ada.text_io;				use ada.text_io;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;

use et_pcb_coordinates.pac_geometry_brd;


procedure rounding is

	use functions_float;

	d1 : type_distance := 0.0;
	d2 : type_distance_coarse;

	mode : type_rounding_mode := DOWN;
	
begin
	put_line ("coarse resolution:" & to_string (d_coarse => type_distance_coarse'small));
	put_line ("rounding mode: " & type_rounding_mode'image (mode));	
	
	for i in 1 .. 21 loop
		new_line;
		put_line ("d1:" & to_string (d1));

		d2 := round (d1, mode);
		
		put_line ("d2:" & to_string (d2));

		d1 := d1 - 0.1 * type_distance_coarse'small;
		--d1 := d1 + 0.01 * type_distance_coarse'small;
	end loop;


end rounding;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
