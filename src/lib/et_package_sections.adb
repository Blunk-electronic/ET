------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PACKAGE SECTIONS                               --
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

--   do do:


with ada.text_io;					use ada.text_io;
with ada.characters.handling;		use ada.characters.handling;


package body et_package_sections is


	function to_string (
		section : in type_package_section) 
		return string 
	is
		s : string := type_package_section'image (section);
	begin
		return s (section_prefix'length + 1 .. s'last);
	end to_string;





-- GENERICS
	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push (x : in item) is
		begin
			top := top + 1;
			s (top) := x;
		end push;

		procedure pop is
		begin
			top := top - 1;
		end pop;
		
		function pop return item is
		begin
			top := top - 1;
			return s (top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;

		function empty return boolean is
		begin
			if top = 0 then return true;
			else return false;
			end if;
		end empty;
		
		function current return item is 
		begin
			return s (top);
		end current;
		
		function parent (degree : in natural := 1) return item is
		begin
			--return s (top - 1);
			return s (top - degree);
		end parent;
		
	end stack_lifo;
	
	
end et_package_sections;
