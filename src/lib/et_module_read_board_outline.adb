------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD / OUTLINE                         --
--                                                                          --
--                               B o d y                                    --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;

with et_module_names;				use et_module_names;
-- with et_module_instance;			use et_module_instance;
with et_board_geometry;				use et_board_geometry;
with et_keywords;					use et_keywords;

with et_module_read_board_contour;	use et_module_read_board_contour;
with et_board_holes;				use et_board_holes;
with et_board_outline;				use et_board_outline;
					
with et_general_rw;					use et_general_rw;



package body et_module_read_board_outline is
	
	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;
		
		

	procedure insert_outline_line is begin
		append_segment (contour, (LINE, contour_line));
		reset_line (contour_line);
	end;

	


	procedure insert_outline_arc is begin
		-- CS board_check_arc (log_threshold + 1);
		
		append_segment (contour, (ARC, contour_arc));
		reset_arc (contour_arc);
	end;



	
	procedure insert_outline_circle is begin
		-- The global contour variable "mutates" so that the contours
		-- consist of a single circle:
		contour := (
			contour	=> (circular => true, others => <>),
			others	=> <>);

		-- From now on the contour consists of just a single circle.
		-- Any attempt to append a line or an arc causes a discriminant error.
		
		-- NOTE: There should not be another circle for the outline,
		-- because only a single circle is allowed.
		
		-- Assign the circle to the contour:
		set_circle (contour, contour_circle);
		reset_circle (contour_circle);
	end;


	
	
	
	
	procedure set_outline (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			module.board.board_contour.outline := 
				(contour with null record);

		end do_it;

	
	begin
		log (text => "module " & to_string (module_cursor)
			& " set outline",
			level => log_threshold);
					
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- CS reset outline
	end;
	
		
		
		
		
		

	procedure add_hole (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 
		use pac_holes;
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			append (
				container 	=> module.board.board_contour.holes,
				new_item	=> (contour with null record));
				
			-- CS procedure add_hole
		end do_it;
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " add hole",
			level => log_threshold);
					
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- CS reset hole
	end;

	
	
	
end et_module_read_board_outline;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
