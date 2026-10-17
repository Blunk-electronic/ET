------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              CONTOURS                                    --
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


package body et_contours is


-- SEGMENTS:


	function to_string (
		segment	: in type_segment)
		return string
	is begin
		case segment.shape is
			when LINE =>
				return ("line segment: " & to_string (segment.segment_line));

			when ARC =>
				return ("arc segment: " & to_string (segment.segment_arc));
		end case;
	end to_string;





	function to_segment (
		line : in type_line)
		return type_segment
	is
		s : constant type_segment := (
			shape => et_contours.LINE,
			segment_line => line);
	begin
		return s;
	end to_segment;



	function to_string (
		segment	: in pac_segments.cursor)
		return string
	is (to_string (element (segment)));





	function is_proposed (
		segment	: in pac_segments.cursor)
		return boolean
	is
		s : constant type_segment := element (segment);
		result : boolean := false;
	begin
		case s.shape is
			when LINE =>
				if is_proposed (s.segment_line) then
					result := true;
				end if;

			when ARC =>
				if is_proposed (s.segment_arc) then
					result := true;
				end if;
		end case;

		return result;
	end is_proposed;



	function is_proposed (
		segment : in type_segment)
		return boolean
	is
		result : boolean := false;
	begin
		case segment.shape is
			when LINE =>
				if is_proposed (segment.segment_line) then
					result := true;
				end if;

			when ARC =>
				if is_proposed (segment.segment_arc) then
					result := true;
				end if;
		end case;

		return result;
	end is_proposed;





	procedure set_proposed (
		segment	: in out type_segment)
	is begin
		case segment.shape is
			when LINE =>
				set_proposed (segment.segment_line);

			when ARC =>
				set_proposed (segment.segment_arc);
		end case;
	end set_proposed;



	procedure clear_proposed (
		segment	: in out type_segment)
	is begin
		case segment.shape is
			when LINE =>
				clear_proposed (segment.segment_line);

			when ARC =>
				clear_proposed (segment.segment_arc);
		end case;
	end clear_proposed;




	function is_selected (
		segment	: in type_segment)
		return boolean
	is
		result : boolean := false;
	begin
		case segment.shape is
			when LINE =>
				if is_selected (segment.segment_line) then
					result := true;
				end if;

			when ARC =>
				if is_selected (segment.segment_arc) then
					result := true;
				end if;
		end case;

		return result;
	end is_selected;




	function is_selected (
		segment	: in pac_segments.cursor)
		return boolean
	is
		s : type_segment renames element (segment);
		result : boolean := false;
	begin
		case s.shape is
			when LINE =>
				if is_selected (s.segment_line) then
					result := true;
				end if;

			when ARC =>
				if is_selected (s.segment_arc) then
					result := true;
				end if;
		end case;

		return result;
	end is_selected;



	procedure set_selected (
		segment	: in out type_segment)
	is begin
		case segment.shape is
			when LINE =>
				set_selected (segment.segment_line);

			when ARC =>
				set_selected (segment.segment_arc);
		end case;
	end set_selected;



	procedure clear_selected (
		segment	: in out type_segment)
	is begin
		case segment.shape is
			when LINE =>
				clear_selected (segment.segment_line);

			when ARC =>
				clear_selected (segment.segment_arc);
		end case;
	end clear_selected;




	function is_moving (
		segment	: in pac_segments.cursor)
		return boolean
	is
		s : type_segment renames element (segment);
		result : boolean := false;
	begin
		case s.shape is
			when LINE =>
				if is_moving (s.segment_line) then
					result := true;
				end if;

			when ARC =>
				if is_moving (s.segment_arc) then
					result := true;
				end if;
		end case;

		return result;
	end is_moving;



	procedure set_moving (
		segment	: in out type_segment)
	is begin
		case segment.shape is
			when LINE =>
				set_moving (segment.segment_line);

			when ARC =>
				set_moving (segment.segment_arc);
		end case;
	end set_moving;



	procedure clear_moving (
		segment	: in out type_segment)
	is begin
		case segment.shape is
			when LINE =>
				clear_moving (segment.segment_line);

			when ARC =>
				clear_moving (segment.segment_arc);
		end case;
	end clear_moving;




	procedure modify_status (
		segment	: in out type_segment;
		operation	: in type_status_operation)
	is begin
		case segment.shape is
			when LINE =>
				modify_status (segment.segment_line, operation);

			when ARC =>
				modify_status (segment.segment_arc, operation);
		end case;
	end modify_status;



	procedure reset_status (
		segment	: in out type_segment)
	is begin
		clear_proposed (segment);
		clear_selected (segment);
		clear_moving (segment);
	end reset_status;









	function get_shape (
		segment	: in type_segment)
		return type_segment_shape
	is (segment.shape);



	function get_shape (
		segment	: in pac_segments.cursor)
		return type_segment_shape
	is (element (segment).shape);




	function in_catch_zone (
		zone	: in type_catch_zone;
		segment : in type_segment)
		return boolean
	is
		result : boolean := false;
	begin
		case get_shape (segment) is
			when LINE =>
				if in_catch_zone (zone, segment.segment_line) then
					result := true;
				end if;

			when ARC =>
				if in_catch_zone (zone, segment.segment_arc) then
					result := true;
				end if;
		end case;

		return result;
	end in_catch_zone;




	function in_catch_zone (
		zone	: in type_catch_zone;
		segment : in pac_segments.cursor)
		return boolean
	is
		result : boolean := false;
	begin
		case get_shape (segment) is
			when LINE =>
				if in_catch_zone (zone, element (segment).segment_line) then
					result := true;
				end if;

			when ARC =>
				if in_catch_zone (zone, element (segment).segment_arc) then
					result := true;
				end if;

				return true;
		end case;

		return result;
	end in_catch_zone;

end et_contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
