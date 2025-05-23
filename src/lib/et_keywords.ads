------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              KEYWORDS                                    --
--                                                                          --
--                               S p e c                                    --
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
--   ToDo: 



package et_keywords is
	
	
	keyword_active					: constant string := "active";	
	keyword_add_level				: constant string := "add_level";
	keyword_alignment				: constant string := "alignment";
	keyword_assembly_technology		: constant string := "technology";
	keyword_assembly_variant		: constant string := "assembly_variant";

	keyword_border_width			: constant string := "border_width";
	keyword_bottom					: constant string := "bottom";

	keyword_center					: constant string := "center";
	keyword_class					: constant string := "class";
	keyword_clearance				: constant string := "clearance";
	keyword_columns					: constant string := "columns";	
	keyword_conductor				: constant string := "conductor";
	keyword_connection				: constant string := "connection";
	keyword_content 				: constant string := "content";

	keyword_default					: constant string := "default";
	keyword_delta					: constant string := "delta";
	keyword_description				: constant string := "description";	
	keyword_destination				: constant string := "destination";
	keyword_device 					: constant string := "device";	
	keyword_diameter				: constant string := "diameter";
	keyword_dielectric				: constant string := "dielectric";	
	keyword_direction 				: constant string := "direction";
	keyword_domain					: constant string := "domain";
	keyword_dots					: constant string := "dots";
	keyword_drill_size				: constant string := "drill_size";
	keyword_dru						: constant string := "dru";

	keyword_easing					: constant string := "easing";
	keyword_easing_radius 			: constant string := "easing_radius";	
	keyword_easing_style  			: constant string := "easing_style";
	keyword_end						: constant string := "end";

	keyword_face					: constant string := "face";	
	keyword_file					: constant string := "file";
	keyword_fill					: constant string := "fill";
	keyword_filled 					: constant string := "filled";
	keyword_fill_style 				: constant string := "fill_style";	
	keyword_flipped					: constant string := "flipped";
	keyword_from					: constant string := "from";

	keyword_generic_name			: constant string := "generic_name";

	keyword_hatching_border_width	: constant string := "hatching_border_width";	
	--keyword_hatching_line_spacing	: constant string := "hatching_line_spacing";		
	keyword_hatching_line_width		: constant string := "hatching_line_width";
	keyword_height					: constant string := "height";	
	keyword_horizontal				: constant string := "horizontal";

	keyword_inner					: constant string := "inner";
	keyword_instance_name			: constant string := "instance_name";
	keyword_isolation				: constant string := "isolation"; -- CS rename
	
	keyword_junction				: constant string := "junction";

	keyword_tag_label				: constant string := "tag_label";
	keyword_layer					: constant string := "layer";	
	keyword_layers					: constant string := "layers";
	keyword_layout					: constant string := "layout";	
	keyword_lines					: constant string := "lines";
	keyword_linewidth				: constant string := "linewidth";
	keyword_locked 					: constant string := "locked";

	keyword_meaning					: constant string := "meaning";
	keyword_micro_via_drill_min		: constant string := "micro_via_drill_min";
	keyword_micro_via_restring_min	: constant string := "micro_via_restring_min";	
	keyword_mirrored				: constant string := "mirrored";
	keyword_model					: constant string := "model";				

	keyword_netchanger				: constant string := "netchanger";		
	keyword_not_mounted				: constant string := "not_mounted";
	
	keyword_off						: constant string := "off";
	keyword_on						: constant string := "on";
	keyword_on_off					: constant string := "on_off";
	keyword_orientation				: constant string := "orientation";
	keyword_origin					: constant string := "origin";
	keyword_outer					: constant string := "outer";

	keyword_package_model 			: constant string := "package_model";
	keyword_pad_shape				: constant string := "pad_shape";	
	keyword_paper_size				: constant string := "paper_size";
	keyword_partcode 				: constant string := "partcode";	
	keyword_path					: constant string := "path";
	keyword_position				: constant string := "position";
	keyword_position_in_board		: constant string := "position_in_board";
	keyword_position_in_schematic	: constant string := "position_in_schematic";	
	keyword_prefix					: constant string := "prefix";
	keyword_priority				: constant string := "priority";

	keyword_radius					: constant string := "radius";	
	keyword_restring_inner 			: constant string := "restring_inner";
	keyword_restring_outer 			: constant string := "restring_outer";
	keyword_revision				: constant string := "revision";
	keyword_rotation 				: constant string := "rotation";
	keyword_rotation_in_schematic	: constant string := "rotation_in_schematic";
	keyword_route					: constant string := "route";
	keyword_rows					: constant string := "rows";

	keyword_scope					: constant string := "scope";	
	keyword_sectors					: constant string := "sectors";
	keyword_sheet					: constant string := "sheet";	
	keyword_sheet_category			: constant string := "category";
	keyword_sheet_description		: constant string := "text";
	keyword_sheet_number			: constant string := "number";
	keyword_size					: constant string := "size";
	keyword_solder_paste_shape			: constant string := "solder_paste_shape";
	keyword_solder_paste_shrink_factor	: constant string := "solder_paste_shrink_factor";
	keyword_solder_paste_status			: constant string := "solder_paste_status";
	keyword_spacing					: constant string := "spacing";
	keyword_start					: constant string := "start";
	keyword_stop_mask_shape			: constant string := "stop_mask_shape";
	keyword_stop_mask_shape_bottom	: constant string := "stop_mask_shape_bottom";	
	keyword_stop_mask_shape_top		: constant string := "stop_mask_shape_top";
	keyword_stop_mask_status		: constant string := "stop_mask_status";
	keyword_style					: constant string := "style";
	keyword_submodule				: constant string := "submodule";
	keyword_swap_level				: constant string := "swap_level";
	keyword_symbol_file				: constant string := "symbol_model";

	keyword_template				: constant string := "template";
	keyword_tht_hole				: constant string := "hole";	
	keyword_to						: constant string := "to";
	keyword_track_width_min			: constant string := "track_width_min";

	keyword_unit					: constant string := "unit";

	keyword_value					: constant string := "value";
	keyword_variant					: constant string := "variant";
	keyword_vertical				: constant string := "vertical";
	keyword_version					: constant string := "version";		
	keyword_via						: constant string := "via";
	keyword_via_category 			: constant string := "category";
	keyword_via_drill				: constant string := "drill";
	keyword_via_drill_min			: constant string := "via_drill_min";
	keyword_via_restring_min		: constant string := "via_restring_min";	
	keyword_view_mode				: constant string := "view_mode";

	keyword_width					: constant string := "width";
	keyword_width_inner_layers		: constant string := "width_inner_layers";
	keyword_x 						: constant string := "x";
	keyword_y 						: constant string := "y";		

	keyword_zone					: constant string := "zone";

	-- keyword_via_restring_inner	: constant string := "restring_inner";
	-- keyword_via_restring_outer	: constant string := "restring_outer";


	
	
end et_keywords;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
