clc
clear all;
close all;

%初始化
num_of_journals = 10;
average_issues_per_year = 12;
num_of_papers_per_issue_a_journal = 10;
num_of_papers_per_journal = 120;
average_reference_per_paper = 30;
average_review_round = 3;
year = 13;
quality_of_manuscript = gamrnd(1,5,600,1);
quality_of_manuscript = quality_of_manuscript-mod(quality_of_manuscript,0.1);
article = zeros(15600,100);
cur_article = 1;

%参数
alpha = 20;
beta = 10;
delta = 10;
gamma = 4;
lambda = 1;
omega = 0.5;
zeta =1.5;
eta = 0.55;
epsilon = 1.5;


%初始影响因子
for i = 1:num_of_journals
    for j = 1:year
        impact_factors = ones(i,j);
    end
end

%初始期刊水平
for i = 1:num_of_journals
    initial_quality_of_journals = round(10*rand(i,1));
end
journal_average_quality = initial_quality_of_journals;

%计算影响因子
month = 1;
for year = 1:13
    for issue = 1:average_issues_per_year
        quantity = zeros(10,200);
        quality = zeros(10,200);
        real_quality = zeros(10,200);
        for w=1:300
            submission(w,1) = quality_of_manuscript(randi(length(quality_of_manuscript)));
            if submission(w,1)>10 || submission(w,1)<1
                submission(w,1) = randi([1,10]);
            end
            submission(w,2) = submission(w,1)*normrnd(1,(zeta/submission(w,1))^eta);%手稿估计水平
            if submission(w,2) >10|| submission(w,2)<1
                submission(w,2) = randi([1,10]);
            end
            submission(w,3) = submission(w,2)-submission(w,1);
            count = 0;
            
            for i = 1:num_of_journals
             
                candidate_journals(i) = 0;
                if submission(w,2)>journal_average_quality(i,year) && submission(w,2) - journal_average_quality(i,year) < 2
                    count = count+1;
                    candidate_journals(count) = i;
                end
            end
            
            last_year = year;
            if year > 2
                last_year = year - 1;
            end
            
            max_candidate_journal_quality = 0.6;
            max_candidate_journal_index = randi([1,num_of_journals]);
            for j2 = 1:count %选水平最高影响因子
                if impact_factors(candidate_journals(j2),last_year)>max_candidate_journal_quality
                    max_candidate_journal_quality = impact_factors(candidate_journals(j2),last_year);
                    max_candidate_journal_index = candidate_journals(j2);
                end
            end
            submission(w,4) = submission(w,1) * normrnd(1,(lambda/max_candidate_journal_quality^eta));
            if count == 0
                submission(w,4) = submission(w,1) * normrnd(1,(lambda/(find(impact_factors(max_candidate_journal_index,last_year)))^omega));
            end
            quantity(max_candidate_journal_index,1) = quantity(max_candidate_journal_index,1) + 1;
            quality(max_candidate_journal_index,quantity(max_candidate_journal_index,1)) = submission(w,4);
            real_quality(max_candidate_journal_index,quantity(max_candidate_journal_index,1)) = submission(w,1);
        end
        for q=1:10
            for row =1:quantity(q)
                for col =1:quantity(q)-row
                    if quality(q,col+1) > quality(q,col)
                        max = quality(q,col); 
                        max = real_quality(q,col);
                        quality(q,col) = quality(q,col+1);
                        real_quality(q,col) = real_quality(q,col+1);
                        quality(q,col+1) = max;
                        real_quality(q,col+1) = max;
                    end
                end
            end
        end
        
        for i =1:num_of_journals
            for j = 1:10
                article(cur_article,1) = real_quality(i,j);
                if article(cur_article,1) == 0;
                    continue;
                end   
                article(cur_article, 2) = month;
                article(cur_article, 3) = i;
                article(cur_article, 4) = rand*average_reference_per_paper*2;
                article(cur_article, 4) = article(cur_article, 4)-mod(article(cur_article, 4),1);
                
                if article(cur_article, 4)<10
                    article(cur_article, 4) = 10;
                end
                article(cur_article, 5) = 0;
                cur_ref=1;
                if month>24
                    while cur_ref<=article(cur_article, 4)
                        dice=rand*cur_article;
                        candidate_ref=dice-mod(dice,1);
                        if candidate_ref<1
                            candidate_ref = 1;
                        end
                        if month-article(candidate_ref,2)>average_review_round %时间对的上
                            probability_to_cite = (1-exp(-article(candidate_ref,1)))*(0.5*tanh((article(candidate_ref, 2)-month+alpha)/beta)+0.5)*tanh((article(candidate_ref, 5)+delta)/gamma)*(tanh((impact_factors(article(candidate_ref, 3),year-1)+epsilon)/4.5));
                            dice = rand;
                            if dice<probability_to_cite
                                article(cur_article, 5+cur_ref) = candidate_ref;
                                article(candidate_ref, 5) = article(candidate_ref, 5)+1;
                                
                                ref_year = article(candidate_ref,2)/12-mod(article(candidate_ref,2)/12,1)+1;
                                ref_journal = article(candidate_ref, 3);
                                if month>24 && ref_year<year && ref_year>year-2
                                    impact_factors(ref_journal, year) = impact_factors(ref_journal, year)+1;
                                end
                                cur_ref = cur_ref+1;
                            end
                        end
                    end
                end
                cur_article = cur_article+1;
            end
        end
        month = month+1;
    end
    if year>2
        for i = 1:num_of_journals
            impact_factors(i,year) = impact_factors(i,year)/(2*num_of_papers_per_journal);
        end
    end
    quality1=zeros(10,1);
    number=zeros(10,1);
    for k=1:10
        for t=1:1200*year
            if article(t,3) == k
                quality1(k,1) =quality1(k,1)+ article(t,1);
                number(k,1)=number(k,1)+1;
            end
        end
        journal_average_quality(k,year+1)=quality1(k,1)/number(k,1);
        journal_average_quality(k,year+1)=(journal_average_quality(k,year+1) + initial_quality_of_journals(k,1))/2;
    end
end





