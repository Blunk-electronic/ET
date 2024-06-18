------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               GEOMETRY                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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

with ada.text_io;				use ada.text_io;

with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;



package body et_geometry_2a is
	
	function get_info (editor: in string)
		return string 
	is 
		use ada.characters.latin_1;
		distance_digits_total : constant positive := type_distance_model'digits;
		distance_digits_right : constant positive := type_distance_model'scale;

		--distance_coarse_digits_total : constant positive := type_distance_model_coarse'digits;
		--distance_coarse_digits_right : constant positive := type_distance_model_coarse'scale;

		rotation_digits_total : constant positive := type_rotation'digits;
		rotation_digits_right : constant positive := type_rotation'scale;
	begin
		--CS put_line ("rounding error:" & pac_geometry_1.to_string (type_float'small));
		-- CS write accuracy
		
		return to_upper (editor & " editor:")
		& lf & "distance fine [mm]"
		& lf & "min:        " & type_distance_model'image (type_distance_model'first)
		& lf & "max:        " & type_distance_model'image (type_distance_model'last)
		& lf & "axis min:   " & type_distance_model'image (axis_min)
		& lf & "axis max:   " & type_distance_model'image (axis_max)
		& lf & "resolution: " & type_distance_model'image (type_distance_model'small)
		& lf & "digits"
		& lf & "left:       " & positive'image (distance_digits_total - distance_digits_right)
		& lf & "right:      " & positive'image (distance_digits_right)
		& lf & "total:      " & positive'image (type_distance_model'digits)
		& lf
		--& lf & "distance coarse [mm]"
		--& lf & "min:        " & type_distance_model_coarse'image (type_distance_model_coarse'first)
		--& lf & "max:        " & type_distance_model_coarse'image (type_distance_model_coarse'last)
		--& lf & "resolution: " & type_distance_model_coarse'image (type_distance_model_coarse'small)
		--& lf & "digits"
		--& lf & "left:       " & positive'image (distance_coarse_digits_total - distance_coarse_digits_right)
		--& lf & "right:      " & positive'image (distance_coarse_digits_right)
		--& lf & "total:      " & positive'image (type_distance_model_coarse'digits)
		--& lf
		& lf & "rotation/angle [degrees (1/360)], mathematical sense, ccw"
		& lf & "min:        " & type_rotation'image (type_rotation'first)
		& lf & "max:        " & type_rotation'image (type_rotation'last)
		& lf & "resolution: " & type_rotation'image (type_rotation'small)
		& lf & "digits"
		& lf & "left:       " & positive'image (rotation_digits_total - rotation_digits_right)
		& lf & "right:      " & positive'image (rotation_digits_right)
		& lf & "total:      " & positive'image (type_rotation'digits)
		& lf
		& lf & "internal float"
		& lf & "min:        " & type_float'image (type_float'first)
		& lf & "max:        " & type_float'image (type_float'last)
		& lf & "digits:     " & positive'image (type_float'digits)
		& lf;
	end get_info;


	
	function to_string (
		distance : in type_distance_model)
		return string
	is begin
		return type_distance_model'image (distance);
	end to_string;

	
	function to_string (
		rotation : in type_rotation_model)
		return string
	is begin
		return type_rotation_model'image (rotation);
	end to_string;

	
	
	function to_string (
		v : in type_vector_model)
		return string
	is begin
		return "x/y: "
			& to_string (v.x) & "/" & to_string (v.y);
	end to_string;


	function invert (
		point	: in type_vector_model)
		return type_vector_model
	is begin
		return (- point.x, - point.y);
	end invert;

	

	procedure move_by (
		point	: in out type_vector_model;
		offset	: in type_vector_model)
	is begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move_by;


	
	function get_distance (
		p1, p2 : in type_vector_model)
		return type_distance_model_positive
	is
		use pac_float_numbers_functions;

		dx : type_float := abs (type_float (p2.x - p1.x));
		dy : type_float := abs (type_float (p2.y - p1.y));
		d : type_float;
	begin
		d := sqrt (dx**2.0 + dy**2.0);
		return type_distance_model_positive (d);
	end get_distance;
	

	function get_angle (
		p1, p2 : in type_vector_model)
		return type_rotation_model
	is
		use pac_float_numbers_functions;

		dx : type_float := type_float (p2.x - p1.x);
		dy : type_float := type_float (p2.y - p1.y);
		a : type_float;
	begin
		-- For a tangens operations, dx must not
		-- be zero. If it is zero, then dy determines
		-- whether the result is 90 or -90 degree:
		if dx /= 0.0 then
			a := arctan (dy, dx, 360.0);
		else
			if dy > 0.0 then
				a := 90.0;
			else
				a := -90.0;
			end if;
		end if;
		
		return type_rotation_model (a);

		exception
			when ADA.NUMERICS.ARGUMENT_ERROR => 
				put_line ("tangens error");
				raise;
		
	end get_angle;



	
	function to_string (
		box : in type_area)
		return string
	is begin
		return "(x/y/w/h): "
			& to_string (box.position.x) & "/"
			& to_string (box.position.y) & "/"
			& to_string (box.width) & "/"
			& to_string (box.height);
	end to_string;

	
	function get_corners (
		area	: in type_area)
		return type_area_corners
	is
		result : type_area_corners;
	begin
		result.BL := (area.position.x, area.position.y);
		result.BR := (area.position.x + area.width, area.position.y);

		result.TL := (area.position.x, area.position.y + area.height); 
		result.TR := (area.position.x + area.width, 
					  area.position.y + area.height); 
		return result;
	end get_corners;


	
	function get_center (
		area	: in type_area)
		return type_vector_model
	is
		result : type_vector_model;
	begin
		result.x := area.position.x + area.width  * 0.5;
		result.y := area.position.y + area.height * 0.5;
		return result;
	end get_center;


	
	function in_area (
		point	: type_vector_model;
		area	: type_area)
		return boolean
	is
		result : boolean := false;
	begin
		-- text x-axis:
		if point.x >= area.position.x then
			if point.x <= area.position.x + area.width then

				-- test y-axis:
				if point.y >= area.position.y then
					if point.y <= area.position.y + area.height then
						result := true;
					end if;
				end if;
				
			end if;
		end if;
		
		return result;
	end in_area;



	

	
	function areas_overlap (
		A, B : in type_area)
		return boolean
	is
		-- CS: Optimization required. Compiler options ?
		-- CS: rename lx, gx, ly, gy to x1, x2, y1, y2
		
		-- AREA A:
		-- This is the lowest x used by area A
		A_lx : type_distance_model renames A.position.x;

		-- This is the greatest x used by area A
		A_gx : constant type_distance_model := A_lx + A.width;

		
		-- This is the lowest y used by area A
		A_ly : type_distance_model renames A.position.y;

		-- This is the greatest y used by area A
		A_gy : constant type_distance_model := A_ly + A.height;


		-- AREA B:
		-- This is the lowest x used by area B
		B_lx : type_distance_model renames B.position.x;

		-- This is the greatest x used by area B
		B_gx : constant type_distance_model := B_lx + B.width;

		
		-- This is the lowest y used by area B
		B_ly : type_distance_model renames B.position.y;

		-- This is the greatest y used by area B
		B_gy : constant type_distance_model := B_ly + B.height;

	begin
		-- If all of the four criteria are true then the two 
		-- areas DO overlap:
		if	B_lx < A_gx 
		and	B_gx > A_lx
		and	B_ly < A_gy
		and	B_gy > A_ly then
			return true;
		else
			return false;
		end if;
	end areas_overlap;


	procedure merge_areas (
		A : in out type_area;
		B : in type_area)
	is
		-- CS: Optimization required. Compiler options ?
		
		-- AREA A:
		-- This is the lowest x used by area A
		A_lx : type_distance_model renames A.position.x;

		-- This is the greatest x used by area A
		A_gx : type_distance_model := A_lx + A.width;

		
		-- This is the lowest y used by area A
		A_ly : type_distance_model renames A.position.y;

		-- This is the greatest y used by area A
		A_gy : type_distance_model := A_ly + A.height;


		-- AREA B:
		-- This is the lowest x used by area B
		B_lx : type_distance_model renames B.position.x;

		-- This is the greatest x used by area B
		B_gx : type_distance_model := B_lx + B.width;

		
		-- This is the lowest y used by area B
		B_ly : type_distance_model renames B.position.y;

		-- This is the greatest y used by area B
		B_gy : type_distance_model := B_ly + B.height;

	begin
		-- x-axis:
		if B_lx < A_lx then
			A_lx := B_lx;
		end if;
		
		if B_gx > A_gx then
			A_gx := B_gx;
		end if;

		-- y-axis:
		if B_ly < A_ly then
			A_ly := B_ly;
		end if;
		
		if B_gy > A_gy then
			A_gy := B_gy;
		end if;

		A.width  := A_gx - A_lx;
		A.height := A_gy - A_ly;
	end merge_areas;

	
	
end et_geometry_2a;

