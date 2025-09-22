------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SCHEMATIC SYMBOL LIBRARY                             --
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

-- with ada.exceptions; 			use ada.exceptions;



package body et_symbol_library is

	

	procedure locate_symbol (
		model_file	: in pac_symbol_model_file.bounded_string;
		cursor		: in out pac_symbols.cursor)
	is begin
		cursor := symbol_library.find (model_file);
	end locate_symbol;

	

	
	
	function is_real (
		symbol : in pac_symbols.cursor)
		return boolean
	is begin
		case element (symbol).appearance is
			when APPEARANCE_PCB		=> return true;
			when APPEARANCE_VIRTUAL	=> return false;
		end case;
	end is_real;



	function get_port_positions (
		symbol	: in pac_symbols.cursor)
		return pac_points.list
	is begin
		return get_port_positions (element (symbol));
	end get_port_positions;

		
	
end et_symbol_library;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
