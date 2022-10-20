function copula_multipliers = get_copula_multipliers(marginals)


    Euv = (min(unique(corr(marginals,'Type','Spearman')))+3)/12;
    
    % define integal (eq 8.9b; Zhang 2019)
    int_term2 = @(a) integral2(@(u,v) exp(-a(1)*(u - 1/2) - a(2)*(u.^2 - 1/3) ...
        - a(3)*(v - 1/2) - a(4)*(v.^2 - 1/3) - a(5)*(u.*v - Euv)),0,1,0,1);
    
    a0 = [1,1,1,1,-1]; %varying only guess for spearman related constrain (most sensible; Zhang 2019)
    
    % solve using fminsearch (lambda 1 2 and 3, gamma 1 and 2)
    obj_fun2 = @(a) int_term2(a);
    a_sol = fminsearch(obj_fun2, a0);
    
    % obtain lambda0 (eq 8.8; Zhang 2019)%
    l0 = integral2(@(u,v) exp(-a_sol(1)*u - a_sol(2)*u.^2 - a_sol(3)*v - ...
        a_sol(4)*v.^2 - a_sol(5)*u.*v),0,1,0,1);
    l0 = log(inv(l0));
    
    copula_multipliers = [l0 a_sol]; %output

end
