module camera_module
    use vec3_module
    use ray_module
    implicit none

    type camera
        type(vec3) :: origin, lower_left_corner
        type(vec3) :: horizontal, vertical
    contains
        procedure :: get_ray
    end type camera

contains

    function init_camera(look_from, look_at, vup, vfov, aspect_ratio)
        type(vec3), intent(in) :: look_from, look_at, vup
        real, intent(in) :: vfov, aspect_ratio
        type(camera) :: init_camera

        real :: theta, h, viewport_height, viewport_width
        type(vec3) :: w, u, v

        theta = vfov * 0.01745329251
        h = tan(theta / 2.0)
        viewport_height = 2.0 * h
        viewport_width = aspect_ratio * viewport_height

        w = unit_vector(look_from - look_at)
        u = unit_vector(cross(vup, w))
        v = cross(w, u)

        init_camera%origin = look_from
        init_camera%horizontal =  u * viewport_width
        init_camera%vertical = v * viewport_height * (-1.0)
        init_camera%lower_left_corner = init_camera%origin - init_camera%horizontal/2.0 - init_camera%vertical/2.0 - w
    end function init_camera

    function get_ray(self, s, t)
        real, intent(in) :: s, t
        class(camera), intent(in) :: self
        type(ray) :: get_ray

        get_ray = ray(self%origin, self%lower_left_corner + self%horizontal * s +  self%vertical * t - self%origin)

    end function get_ray
    
end module camera_module