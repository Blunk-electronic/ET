------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DRAWING FRAME                                  --
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

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;


package body et_drawing_frame is

	function to_paper_size (paper_size : in string) return type_paper_size is begin
		return type_paper_size'value (paper_size);
	end;
	
	function to_string (paper_size : in type_paper_size) return string is begin
		return type_paper_size'image (paper_size);
	end;

	
	function to_string (orientation : in type_orientation) return string is begin
		return to_lower (type_orientation'image (orientation));
	end;

	function to_orientation (orientation : in string) return type_orientation is begin
		return type_orientation'value (orientation);
	end;


	function to_string (rows : in type_rows) return string is begin
		return trim (type_rows'image (rows), left);
	end;

	function to_rows (rows : in string) return type_rows is begin
		return type_rows'value (rows);
	end;

	function to_string (columns : in type_columns) return string is begin
		return trim (type_columns'image (columns), left);
	end;

	function to_columns (columns : in string) return type_columns is begin
		return type_columns'value (columns);
	end;


	
	

	function to_string (
		distance : in type_distance) 
		return string 
	is begin
		return type_distance'image (distance);
	end;



	function to_distance (
		distance : in string)
		return type_distance
	is begin
		return type_distance'value (distance);
	end;

	


	
	function paper_dimension (
		paper_size	: in type_paper_size;
		orientation	: in type_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance_positive 
	is
		dimension : type_distance_positive;	
	begin
		case orientation is
			when LANDSCAPE =>
				case paper_size is 
					when A3 =>
						case axis is
							when AXIS_X => dimension := paper_size_A3_x;
							when AXIS_Y => dimension := paper_size_A3_y;
						end case;

					when A4 =>
						case axis is
							when AXIS_X => dimension := paper_size_A4_x;
							when AXIS_Y => dimension := paper_size_A4_y;
						end case;
				end case;

			when PORTRAIT =>
				case paper_size is 
					when A3 =>
						case axis is
							when AXIS_X => dimension := paper_size_A3_y;
							when AXIS_Y => dimension := paper_size_A3_x;
						end case;

					when A4 =>
						case axis is
							when AXIS_X => dimension := paper_size_A4_y;
							when AXIS_Y => dimension := paper_size_A4_x;
						end case;
				end case;

		end case;

		return dimension;
	end paper_dimension;



	function add (
		right, left : in type_position)
		return type_position
	is
		result : type_position;
	begin
		result.x := right.x + left.x;
		result.y := right.y + left.y;
		return result;
	end add;


	
	
	function to_string (
		p 		: in type_position;
		format	: in type_output_format := FORMAT_1)
		return string
	is begin
		case format is
			when FORMAT_1 =>
				return ("x/y " & to_string (p.x) & "/ " & to_string (p.y));

			when FORMAT_2 =>
				return ("x " & to_string (p.x) & " y " & to_string (p.y));

			when FORMAT_3 =>
				return (to_string (p.x) & " " & to_string (p.y));
		end case;
	end to_string;

	


	function to_string (name : in pac_template_name.bounded_string) return string is begin
		return pac_template_name.to_string (name);
	end;


	
	function to_template_name (name : in string) return pac_template_name.bounded_string is begin
		return pac_template_name.to_bounded_string (name);
	end;


	
	function to_string (domain : in type_domain) return string is
		s : string := type_domain'image (domain);
	begin
		return s (domain_prefix'length + 1 .. s'last);
	end;


	
	function to_domain (domain : in string) return type_domain is begin
		return type_domain'value (domain_prefix & domain);
	end;



	
	procedure set_position (
		frame 		: in out type_frame_general;
		position	: in type_position)
	is begin
		frame.position := position;
	end set_position;



	
	function get_position (
		frame 		: in type_frame_general)
		return type_position
	is begin
		return frame.position;
	end get_position;



	
end et_drawing_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
