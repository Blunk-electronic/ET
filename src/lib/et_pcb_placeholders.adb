------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PLACEHOLDERS ON THE PCB                         --
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
--   to do:


with ada.characters.handling;		use ada.characters.handling;


package body et_pcb_placeholders is
	


	function to_string (
		meaning : in type_text_meaning_conductor) 
		return string 
	is begin
		return to_lower (type_text_meaning_conductor'image (meaning));
	end to_string;


	
	function to_meaning (
		meaning : in string) 
		return type_text_meaning_conductor 
	is begin
		return type_text_meaning_conductor'value (meaning);
	end to_meaning;



	
	function get_layer (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return type_signal_layer
	is begin
		return element (placeholder).layer;
	end get_layer;



	function is_selected (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return boolean
	is begin
		if is_selected (element (placeholder)) then
			return true;
		else
			return false;
		end if;
	end is_selected;
	


	function is_proposed (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return boolean
	is begin
		if is_proposed (element (placeholder)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;

	
	
end et_pcb_placeholders;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
