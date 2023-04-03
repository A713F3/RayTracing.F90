program raytracing
    use vec3_module
    use ray_module
    use sphere_module
    use ppm

    implicit none

    type(vec3) :: white, background, main_color 
    type(vec3) :: pi, L, N, pix_col

    type(sphere) :: sph, light

    type(ray) :: ry

    real :: t, dt

    integer :: H, W, x, y

    logical :: intersects

    white = vec3(255.0, 255.0, 255.0)
    background = vec3(30.0, 30.0, 30.0)
    main_color = vec3(190.0, 239.0, 85.0)

    sph = sphere(vec3(250.0, 250.0, 100.0), 100.0)
    light = sphere(vec3(0.0, 0.0, 50.0), 1.0)

    t = 0.0

    H = 500
    W = 500

    call ppm_init(W, H)

    do y=1, H
        do x=1, W
            pix_col = background

            ry = ray(vec3(real(x), real(y), 0.0), vec3(0.0, 0.0, 1.0))
            call intersect(sph, ry, t, intersects)

            if (intersects) then
                pi = ry%o + ry%d * t
                L = light%c - pi
                N = get_normal(sph, pi)
                dt = dot(normalize(L), normalize(N))

                pix_col = (main_color + white*dt) * 0.5
                pix_col = clamp(255.0, pix_col)
            end if

            call point(x, y, color(int(pix_col%x), int(pix_col%y), int(pix_col%z)))
        end do
    end do

    call render_image(1, "o", .true.)

    call ppm_release()

end program raytracing