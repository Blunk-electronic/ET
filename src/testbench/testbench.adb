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
with ada.strings.unbounded;

with ada.numerics.generic_elementary_functions;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;

use et_pcb_coordinates.pac_geometry_brd;
use et_board_shapes_and_text.pac_shapes;


procedure testbench is

	type type_place is (BEFORE, AFTER);
						
	place : type_place := AFTER;
	
	c : type_circle := (
				center => type_point (set (11.6201, 0.0)),
				radius => 0.9);

	--C2 : type_circle := (
				--center => type_point (set (66.621, 0.0)),
				--radius => 0.9);

	
	S : type_point := type_point (set (x => 0.7201, y => 0.5400));
	E : type_point := type_point (set (x => 10.7201, y => 95.5400)); 
	-- dx : 10
	-- dy : 95
	
	line_tmp : type_line := (S, E);

	--d : type_distance;

	-- the distance between center of cap and line:
	d_cap_to_line : type_distance;
	d_cap_to_line_abs : type_distance_positive;

	c_bak : type_circle;
	step : type_distance_positive := 0.5;
	
	-- There is a maximum of iterations. If maximum reached
	-- a constraint_error is raised.
	max_iterations : constant positive := 100; -- CS increase if necessary
	--max_iterations : constant positive := 1000000; -- CS increase if necessary

	dyn_width : boolean := true;

	procedure set_step_width is begin
		if dyn_width then
			step := d_cap_to_line_abs * 0.5;
		else
			step := type_distance'small;
		end if;
	end set_step_width;

begin

	--d := get_distance (C1, L);

	--new_line;

	--d := get_distance (C2, L);	
	--put_line (to_string (d));

	for i in 1 .. max_iterations loop

		
		-- Calculate the distance between the cap (incl. clearance) and the line:
		d_cap_to_line := get_distance (c, line_tmp);
		put_line ("distance" & to_string (d_cap_to_line));

		d_cap_to_line_abs := abs (d_cap_to_line);
		
		-- Cancel this loop once the distance is sufficiently small.
		-- Otherwise take half of the distance and move cap to new position:
		if d_cap_to_line_abs <= type_distance'small then
			put_line (" break point found after" & positive'image (i) & " iterations");
			exit;
		else
			--d_cap_to_line_abs := d_cap_to_line_abs * 0.5;
			--d_cap_to_line_abs := d_cap_to_line_abs * step;
			--d_cap_to_line_abs := type_distance'small;
			
			case place is
				when BEFORE =>
					if d_cap_to_line > zero then
						-- move cap right towards the line:
						set_step_width;								
						c_bak := c;
						c.center := type_point (move (c.center, 0.0, step));
						--c.center := type_point (move (c.center, 0.0, type_distance'small));
					else
						-- move cap left away from the line:
						--c.center := type_point (move (c.center, 180.0, type_distance'small));
						c := c_bak;
						dyn_width := false;
					end if;
					
				when AFTER =>
					if d_cap_to_line > zero then
						-- move cap left towards the line:
						set_step_width;
						c_bak := c;
						c.center := type_point (move (c.center, 180.0, step));
						--c.center := type_point (move (c.center, 180.0, type_distance'small));
					else
						-- move cap right away from the line:
						--c.center := type_point (move (c.center, 0.0, type_distance'small));
						c := c_bak;
						dyn_width := false;
					end if;
			end case;
		end if;

		
		-- Once the maximum of iterations has been reached, raise exception:
		if i = max_iterations then
			raise constraint_error with "ERROR: Max. interations of " & positive'image (i) &
			" reached !";
		end if;
	end loop;

	
end testbench;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
