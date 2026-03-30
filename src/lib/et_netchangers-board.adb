------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         NETCHANGERS / BOARD                              --
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
-- To Do: 
--
--

with ada.text_io;					use ada.text_io;

with ada.characters;				use ada.characters;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
-- with ada.strings.fixed;				use ada.strings.fixed;
-- with ada.strings.bounded;      		use ada.strings.bounded;
-- with ada.strings.maps;				use ada.strings.maps;

with et_coordinates_formatting;		use et_coordinates_formatting;



package body et_netchangers.board is


	function to_string (
		position	: in type_netchanger_position_board)
		return string
	is begin
		return to_string (position.place) 
			& " layer " & to_string (position.layer);
	end;



	procedure set_place (
		netchanger	: in out type_netchanger;
		place		: in type_vector_model)
	is begin
		netchanger.position_brd.place := place;
	end;



	function get_place (
		netchanger	: in type_netchanger)
		return type_vector_model
	is begin
		return netchanger.position_brd.place;
	end;




	procedure set_layer (
		netchanger	: in out type_netchanger;
		layer		: in type_signal_layer)
	is begin
		netchanger.position_brd.layer := layer;
	end;
	
	
	
	function get_layer (
		netchanger	: in type_netchanger)
		return type_signal_layer
	is begin
		return netchanger.position_brd.layer;
	end;

	
	
end et_netchangers.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
