------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              TESTBENCH                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with ada.strings.unbounded;		use ada.strings.unbounded;

with et_geometry;				use et_geometry;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_string_processing;		use et_string_processing;

procedure clean_up is

	use pac_geometry_brd;

	f_list, f_list_bak : pac_float_numbers.list;



	procedure do_it (mode : in type_clean_up_mode) is begin
		put_line ("mode: " & type_clean_up_mode'image (mode));
		
		put_line ("in:");
		put_line (to_string (f_list));
		
		clean_up (
			numbers	=> f_list,
			mode	=> mode);

		put_line ("out:");
		put_line (to_string (f_list));
	end do_it;

		
begin

	--f_list.append (-0.19);
	
	for i in 1 .. 3 loop
		f_list.append (1.0);
	end loop;

	f_list.append (3.0);

	for i in 1 .. 2 loop
		f_list.append (5.0);
	end loop;


	--f_list.append (-0.7);
	
	f_list_bak := f_list;
	do_it (REDUCE_TO_ONE);


	f_list := f_list_bak;
	do_it (REMOVE_REDUNDANT);


	f_list.clear;
	do_it (REDUCE_TO_ONE);
	do_it (REMOVE_REDUNDANT);

	
	f_list.append (1.0);
	do_it (REDUCE_TO_ONE);
	do_it (REMOVE_REDUNDANT);

	
end clean_up;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
