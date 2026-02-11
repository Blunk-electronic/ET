------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SYMBOL PORT / GENERAL PROPERTIES                     --
--                                                                          --
--                              B o d y                                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--

--
--
--   history of changes:
--
--
--
-- To Do:
--
--


package body et_symbol_port_general is


	
	function get_place (
		port 		: in type_port_general)
		return type_vector_model
	is begin
		return port.position;
	end;


		
	procedure set_place (
		port		: in out type_port_general;
		place		: in type_vector_model)
	is begin
		port.position := place;
	end;
	


	
	function get_rotation (
		port		: in type_port_general)
		return type_rotation_relative
	is begin
		return port.rotation;
	end;



	procedure set_rotation (
		port		: in out type_port_general;
		rotation	: in type_rotation_relative)
	is begin
		port.rotation := rotation;
	end;

	


	function get_length (
		port		: in type_port_general)
		return type_port_length
	is begin
		return port.length;
	end;
		

	
	
end et_symbol_port_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
