------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE WRITE / META                              --
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
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;


with et_assembly_technology;			use et_assembly_technology;
with et_package_bom_relevance;			use et_package_bom_relevance;
with et_package_description;			use et_package_description;
with et_keywords;						use et_keywords;
with et_file_sections;					use et_file_sections;

with et_file_write;						use et_file_write;


package body et_package_write_meta is

	

	procedure write_meta (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is begin
		log (text => "write meta data", level => log_threshold);
		
		write (keyword => keyword_description, wrap => true, 
			   parameters => to_string (packge.description));

		write (keyword => keyword_bom_relevant, 
			   parameters => to_string (packge.appearance));
		
		write (keyword => keyword_assembly_technology, 
			   parameters => to_string (packge.technology));


	end write_meta;
			
	
end et_package_write_meta;
