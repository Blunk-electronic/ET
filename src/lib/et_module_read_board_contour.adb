------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD CONTOUR                           --
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

with et_keywords;					use et_keywords;

with et_general_rw;					use et_general_rw;



package body et_module_read_board_contour is
	
	
	
	procedure read_contour_line (
		line : type_fields_of_line)
	is
		kw : constant string := f (line, 1);
		vm : type_vector_model;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_A (contour_line, vm);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			vm := to_vector_model (line, 2);
			set_B (contour_line, vm);
			
		else
			invalid_keyword (kw);
		end if;
	end;


	

	procedure insert_contour_line is begin
		append_segment (contour, (LINE, contour_line));
		reset_line (contour_line);
	end;

	


	procedure insert_contour_arc is begin
		-- CS board_check_arc (log_threshold + 1);
		
		append_segment (contour, (ARC, contour_arc));
		reset_arc (contour_arc);
	end;



	
	procedure insert_contour_circle is begin
		-- The global contour variable "mutates" so that the contours
		-- consist of a single circle:
		contour := (
			contour	=> (circular => true, others => <>),
			others	=> <>);

		-- From now on the contour consists of just a single circle.
		-- Any attempt to append a line or an arc causes a discriminant error.
		
		-- Assign the circle to the contour:
		set_circle (contour, contour_circle);
		reset_circle (contour_circle);
	end;


	

	procedure check_contour (
		log_threshold : in type_log_level)
	is 
		status : constant type_contour_status := is_closed (contour);
	begin		
		log (text => "check outline", level => log_threshold);
		log_indentation_up;
		
		if status.closed then
			null;
		else
			log (WARNING, "Contour not properly closed at: " 
				& to_string (status.gaps));
			-- CS: write implications and dangers !
		end if;

		log_indentation_down;		
	end;

	
	
end et_module_read_board_contour;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
