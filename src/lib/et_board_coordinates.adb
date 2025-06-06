------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD COORDINATES                               --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.exceptions;

with et_coordinates_formatting;			use et_coordinates_formatting;


package body et_board_coordinates is

	
	function to_string (
		p : in type_package_position) 
		return string 
	is begin
		-- return position_preamble
		-- 		& to_string (get_x (p))
		-- 		& axis_separator
		-- 		& to_string (get_y (p))
		-- 		& axis_separator
		-- 		& to_string (get_rotation (p))
		-- 		& axis_separator
		-- 		& to_string (p.face);

		return (" position" & to_string (p.place)
			& " angle" & to_string (get_rotation (p))
			& " face" & to_string (get_face (p)));
		
	end to_string;

	
	function to_package_position (
		point 		: in type_vector_model;
		rotation	: in type_rotation_model := zero_rotation;
		face		: in type_face := TOP)
		return type_package_position 
	is begin
		return pos : type_package_position do
			set (pos, point);
			set (pos, rotation);
			pos.face := face;
		end return;
	end to_package_position;

	
	procedure set_face (
		face	: in type_face;
		position: in out type_package_position) 
	is begin
		position.face := face;
	end set_face;

	
	function get_face (
		packge : in type_package_position)
		return type_face 
	is begin
		return packge.face;
	end get_face;
	

	procedure flip (
		position : in out type_package_position)
	is begin
		case get_face (position) is
			when TOP => set_face (BOTTOM, position);
			when BOTTOM => set_face (TOP, position);
		end case;
	end;

	
	function get_place (
		position : in type_package_position)
		return type_vector_model
	is begin
		return position.place;
	end get_place;


	function get_position (
		position : in type_package_position)
		return type_position
	is begin
		return type_position (position);
	end get_position;

	
	
	function to_terminal_position (
		point		: in type_vector_model;
		rotation	: in type_rotation_model)
		return type_position'class 
	is
		pos : type_position;
	begin
		--pos := (point with rotation);
		set (pos, point);
		set (pos, rotation);
		return pos;
	end to_terminal_position;
	
end et_board_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
