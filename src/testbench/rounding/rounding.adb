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
with et_pcb_coordinates;		use et_pcb_coordinates;
--with et_board_shapes_and_text;	use et_board_shapes_and_text;

procedure rounding is

use et_pcb_coordinates.pac_geometry_brd;

	
	d : type_distance;
	f : type_float_internal;
	d2 : type_distance;

	S : type_distance := 0.0; --1.0000000007;
	E : type_distance := 1.000;
	I : type_distance_positive := 0.0000000001;
	M : type_float_internal    := 0.00000000001;

	log_on : boolean := true;

	procedure log_result is begin
		put_line ("d:"  & to_string (d) & " f:"  & type_float_internal'image (f)
				& " d2:" & to_string (d2));
	end log_result;


	function to_distance_2 (f : in type_float_internal)
		return type_distance 
	is
		use pac_distance_io;
	begin
		--return 1.05 * type_distance (f);
		return type_distance (f);
		--if f < 0.0 then
			--declare
				--r : string (1 .. type_distance'digits + 2); -- sign + point
			--begin
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_distance'scale, exp => 0);
				--return type_distance'value (r);
			--end;
		--else
			--declare
				--r : string (1 .. type_distance'digits + 1); -- point
			--begin
				----put_line (type_float_internal'image (f) & " " & natural'image (r'length));
				---- CS: IMPROVEMENT REQUIRED !!!
				--put (to => r, item => f, aft => type_distance'scale, exp => 0);
				--return type_distance'value (r);
			--end;
		--end if;
	end to_distance_2;


	
begin
	--log_on := false;
	d := S;
	
	while d < E loop
		f := type_float_internal (d);

		f := f * M;
		--d2 := to_distance (f);
		d2 := to_distance_2 (f) / type_distance (M);
		
		if log_on then
			log_result;
		end if;
		
		if d /= d2 then
			put_line ("ERROR");
			log_result;
			exit;
		end if;
			
		d := d + I;
		--d := d * M;
	end loop;


end rounding;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
